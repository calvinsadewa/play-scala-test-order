package models

import java.util.Date
import javax.inject.Inject

import akka.event.slf4j.Logger
import anorm.SqlParser._
import anorm._
import models.OrderInBrowseAccessLayer.ErrorCode
import play.api.db.DBApi

import scala.concurrent.Future
import play.api.libs.json._

case class ProductAmount(ProductId: Int,Amount: Int)
case class OrderProfile(name: String, phone_number: String, email: String, address: String)
case class OrderInBrowse(UserId: Int,
                         OrderData: List[ProductAmount] = List(),
                         Version: Int = 0,
                         CouponId: Option[Int] = None, Profil: Option[OrderProfile] = None)

object OrderInBrowseAccessLayer {
  object ErrorCode {
    val ProductAmountBelowZero = 0
    val NotLastestVersion = 1
  }
}

object OrderJsonConverter {
  implicit val productAmountReads = Json.reads[ProductAmount]
  implicit val productAmountWrite = Json.writes[ProductAmount]
  implicit val orderProfileReads = Json.reads[OrderProfile]
  implicit val orderProfileWrite = Json.writes[OrderProfile]
  implicit val orderInBrowseReads = Json.reads[OrderInBrowse]
  implicit val orderInBrowseWrite = Json.writes[OrderInBrowse]
}

@javax.inject.Singleton
class OrderInBrowseAccessLayer @Inject()(dbapi: DBApi)(implicit ec: DatabaseExecutionContext) {
  import OrderJsonConverter._
  import play.api.libs.json.Reads._
  import play.api.libs.json.Writes._

  private val db = dbapi.database("default")

  private val simple = {
    get[Int]("user_id") ~
      get[String]("order_data") ~
      get[Int]("version") ~
      get[Option[Int]]("coupon_id") ~
      get[Option[String]]("profile")map {
      case uid~data~version~cid~profile =>
        OrderInBrowse(uid, Json.fromJson[List[ProductAmount]](Json.parse(data)).get, version, cid,
          profile.map(x => Json.fromJson[OrderProfile](Json.parse(x)).get))
    }
  }

  // Get order in browse by user_id
  // create one if not found
  def getByUserId(userId: Int): Future[OrderInBrowse] = Future {
    db.withTransaction { implicit connection =>
      val result = SQL("select * from OrderInBrowse where user_id = {id}").on("id" -> userId).as(simple.singleOpt)
      result.getOrElse {
        val newOrder = OrderInBrowse(userId)
        SQL("insert into OrderInBrowse (user_id, order_data, version) values ({user_id}, {data}, {version})")
          .on('user_id -> userId,
            'data -> Json.stringify(Json.toJson(newOrder.OrderData)),
            'version -> newOrder.Version)
          .executeInsert()
        newOrder
      }
    }
  }(ec)

  // Add product to order in browse
  // return Either, left for error code, right for success version number:
  // ErrorCode:
  // ProductAmountBelowZero
  // NotLastestVersion
  def addProductToOrder(userId: Int, productAmount: ProductAmount,version: Int): Future[Either[Int,Int]] = {
    val getOrder = getByUserId(userId)
    val addOrder = getOrder.map { order =>
      val dataMap = order.OrderData.map(d => d.ProductId -> d.Amount).toMap
      val newDataMap = dataMap.updated(
        productAmount.ProductId,
        dataMap.getOrElse(productAmount.ProductId,0) + productAmount.Amount)
      val newData = newDataMap.map( x => ProductAmount(x._1,x._2)).toList
      if (newDataMap.getOrElse(productAmount.ProductId,0) < 0)
        Left(ErrorCode.ProductAmountBelowZero)
      else
        Right(order.copy(OrderData = newData))
    }
    val updateDB = addOrder flatMap {
      _ match {
        case Left(err) => Future(Left(err))
        case Right(order) => Future {
          db.withConnection { implicit connection =>
            val aff_rows = SQL("""
                UPDATE OrderInBrowse
                SET order_data = {data}, version = {new_version}
                WHERE user_id = {id} and version = {version}""")
              .on("data" -> Json.stringify(Json.toJson(order.OrderData)),
                "id" -> order.UserId,
                "version" -> version,
                "new_version" -> (version + 1)).executeUpdate()
            if (aff_rows == 0)
              Left(ErrorCode.NotLastestVersion)
            else
              Right(version + 1)
          }
        }(ec)
      }
    }
    updateDB
  }

  // add coupon to order in browse
  def applyCouponToOrder(userId: Int, couponId: Int): Future[Unit] = getByUserId(userId).map{ _ =>
    db.withConnection { implicit connection =>
      val aff_rows = SQL("""
                UPDATE OrderInBrowse
                SET coupon_id = {cid}
                WHERE user_id = {id}""")
        .on('cid -> couponId, 'id -> userId).executeUpdate()
    }
  } (ec)

  // set profile to order in browse
  def changeProfile(userId: Int,profile: OrderProfile): Future[Unit] = getByUserId(userId).map{ _ =>
    db.withConnection { implicit connection =>
      val aff_rows = SQL("""
                UPDATE OrderInBrowse
                SET profile = {profile}
                WHERE user_id = {id}""")
        .on('id -> userId,
          'profile -> Json.stringify(Json.toJson(Some(profile)))).executeUpdate()
    }
  } (ec)

  // delete order in browse
  def delete(userId: Int) = Future {
    db.withConnection { implicit connection =>
      SQL("DELETE FROM OrderInBrowse WHERE user_id = {id}").on('id -> userId).executeUpdate()
    }
  } (ec)
}