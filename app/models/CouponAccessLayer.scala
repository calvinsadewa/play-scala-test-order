package models
import javax.inject.Inject

import akka.event.slf4j.Logger
import anorm.SqlParser._
import anorm._
import org.joda.time.DateTime
import play.api.db.DBApi
import play.api.libs.json.Json

import scala.concurrent.Future

object CouponAccessLayer{
  case class Coupon(Id:Int, PortionCut: Double, NominalCut: Double, ValidStart: DateTime, ValidEnd: DateTime)
  object ErrorCode {
    val CouponNotValid = 0
    val UserDoNotHaveCoupon = 1
    val CouponNotFound = 2
  }
}

@javax.inject.Singleton
class CouponAccessLayer @Inject()(dbapi: DBApi)(implicit ec: DatabaseExecutionContext){
  private val db = dbapi.database("default")
  import CouponAccessLayer._
  import CouponAccessLayer.ErrorCode._
  import extensions.NiceFutureEither._

  private val couponParser = {
    get[Int]("id") ~
      get[Double]("portion_cut") ~
      get[Double]("nominal_cut") ~
      get[Long]("valid_start") ~
      get[Long]("valid_end")map {
      case id~pcut~ncut~vstart~vend=>
        Coupon(id, pcut, ncut, new DateTime(vstart), new DateTime(vend))
    }
  }

  // Check if coupon is valid for transaction
  def checkCoupon(couponId: Int, userId: Int) = getCoupon(couponId)
    .map(_ match {
      case Some(x) => Right(x)
      case None => Left(CouponNotFound)
    }).map { _.right.flatMap { coupon =>
      if (coupon.ValidStart.isBeforeNow() && coupon.ValidEnd.isAfterNow) Right(coupon)
      else Left(CouponNotValid)
    }}.map { _.right.map { coupon =>
      getCouponUserAmount(couponId, userId).map { amount =>
        if (amount <= 0) Left(UserDoNotHaveCoupon)
        else Right(coupon)
      }
    }.toFuture.map(_.right.flatMap(x => x))}.flatten

  // get coupon
  def getCoupon(couponId:Int) = Future {
    db.withConnection( implicit connection =>
      SQL("SELECT * FROM coupons WHERE id = {cid}").on('cid -> couponId)
        .as(couponParser.singleOpt)
    )
  }(ec)

  // set coupon
  def setCoupon(coupon: Coupon) = Future {
    db.withTransaction { implicit connenction =>
      SQL("DELETE FROM coupons WHERE id={cid}").on('cid -> coupon.Id).execute()
      SQL("INSERT INTO coupons (id,portion_cut, nominal_cut, valid_start, valid_end) VALUES ({cid}, {pcut}, {ncut}, {vstart}, {vend})")
        .on('cid -> coupon.Id,
          'pcut -> coupon.PortionCut,
          'ncut -> coupon.NominalCut,
          'vstart -> coupon.ValidStart.getMillis(),
          'vend -> coupon.ValidEnd.getMillis()).executeInsert()
    }
  }(ec)

  // set coupon_user
  def setCouponUser(couponId: Int, userId: Int, amount: Int) = Future {
    db.withTransaction { implicit connenction =>
      SQL("DELETE FROM coupon_users WHERE coupon_id={cid} and user_id={uid}")
        .on('cid -> couponId,
          'uid -> userId).execute()
      SQL("INSERT INTO coupon_users (coupon_id,user_id, amount) VALUES ({cid}, {pid}, {amount})")
        .on('cid -> couponId,
          'pid -> userId,
          'amount -> amount).executeInsert()
    }
  }(ec)

  // get coupon amount for user
  def getCouponUserAmount(couponId: Int, userId: Int) = Future {
    db.withConnection( implicit connection =>
      SQL("SELECT amount FROM coupon_users WHERE coupon_id = {cid} and user_id = {uid}")
        .on('cid -> couponId, 'uid -> userId)
        .as(scalar[Int].singleOpt).getOrElse(0)
    )
  }(ec)

  // use coupon
  def useCoupon(couponId: Int, userId: Int) = getCouponUserAmount(couponId, userId).map(
    amount => db.withConnection( implicit connection =>
      SQL("UPDATE coupon_users SET amount={new_amount} WHERE coupon_id = {cid} and user_id = {uid}")
        .on('cid -> couponId, 'uid -> userId, 'new_amount -> (amount - 1)).executeUpdate()
    )
  )

  // cancel use coupon
  def cancelUseCoupon(couponId: Int, userId: Int) = getCouponUserAmount(couponId, userId).map(
    amount => db.withConnection( implicit connection =>
      SQL("UPDATE coupon_users SET amount={new_amount} WHERE coupon_id = {cid} and user_id = {uid}")
        .on('cid -> couponId, 'uid -> userId, 'new_amount -> (amount + 1)).executeUpdate()
    )
  )
}
