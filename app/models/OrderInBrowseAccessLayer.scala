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
                         OrderData: List[ProductAmount],
                         Version: Int,
                         CouponId: Option[Int])

object OrderInBrowseAccessLayer {
  object ErrorCode {
    val ProductAmountBelowZero = 0
    val NotLastestVersion = 1
  }
}

@javax.inject.Singleton
class OrderInBrowseAccessLayer @Inject()(dbapi: DBApi)(implicit ec: DatabaseExecutionContext) {


  implicit val productAmountReads = Json.reads[ProductAmount]
  implicit val productAmountWrite = Json.writes[ProductAmount]
  private val db = dbapi.database("default")

  private val simple = {
    get[Int]("user_id") ~
      get[String]("order_data") ~
      get[Int]("version") ~
      get[Option[Int]]("coupon_id") map {
      case uid~data~version~cid =>
        OrderInBrowse(uid, Json.fromJson[List[ProductAmount]](Json.parse(data)).get, version, cid)
    }
  }

  // Get order in browse by user_id
  // create one if not found
  def getByUserId(userId: Int): Future[OrderInBrowse] = Future {
    db.withTransaction { implicit connection =>
      val result = SQL("select * from OrderInBrowse where user_id = {id}").on("id" -> userId).as(simple.singleOpt)
      result.getOrElse {
        val newOrder = OrderInBrowse(userId, List(), 0, None)
        SQL("insert into OrderInBrowse values ({user_id}, {data}, {version})")
          .on('user_id -> userId,
            'data -> Json.stringify(Json.toJson(newOrder.OrderData)),
            'version -> 0)
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


}