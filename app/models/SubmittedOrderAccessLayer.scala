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

case class SubmittedOrder(Id : Int,
                          UserId: Int,
                           OrderData: List[ProductAmount] = List(),
                           CouponId: Option[Int],
                           Profil: OrderProfile,
                           TotalPrice: Double,
                           Verified: Boolean,
                           Canceled: Boolean,
                           PaymentProof: Option[Int],
                           ShippingId: Option[Int])

object SubmittedOrderAccessLayer {
  object JsonConverter {
    implicit val productAmountReads = Json.reads[ProductAmount]
    implicit val productAmountWrite = Json.writes[ProductAmount]
    implicit val orderProfileReads = Json.reads[OrderProfile]
    implicit val orderProfileWrite = Json.writes[OrderProfile]
    implicit val submittedOrderReads = Json.reads[SubmittedOrder]
    implicit val submittedOrderWrite = Json.writes[SubmittedOrder]
  }
}

@javax.inject.Singleton
class SubmittedOrderAccessLayer @Inject()(dbapi: DBApi)(implicit ec: DatabaseExecutionContext) {
  import SubmittedOrderAccessLayer.JsonConverter._
  import play.api.libs.json.Reads._
  import play.api.libs.json.Writes._

  private val db = dbapi.database("default")

  private val submittedOrderParser = {
    get[Int]("id") ~
      get[Int]("user_id") ~
      get[String]("order_data") ~
      get[Option[Int]]("coupon_id") ~
      get[String]("profile") ~
      get[Double]("total_price") ~
      get[Boolean]("verified") ~
      get[Boolean]("canceled") ~
      get[Option[Int]]("payment_proof") ~
      get[Option[Int]]("shipping_id")map {
      case id~uid~data~cid~profile~total_price~verified~canceled~payment_proof~shipping_id =>
        SubmittedOrder(id,uid, Json.fromJson[List[ProductAmount]](Json.parse(data)).get, cid,
          Json.fromJson[OrderProfile](Json.parse(profile)).get,total_price,verified,canceled,payment_proof,shipping_id)
    }
  }

  // add new submitted order
  // return possible id
  def addNewOrder(userId: Int,
                  orderData: List[ProductAmount],
                  couponId: Option[Int],
                  profil:OrderProfile, totalPrice: Double): Future[Int] = Future {
    import anorm.SqlParser.int
    db.withConnection{ implicit connection =>
      SQL(
        """
          insert into submitted_order (user_id, order_data, coupon_id, profile, total_price,
          verified,canceled,payment_proof,shipping_id)
          values ({user_id}, {order_data}, {coupon_id}, {profile}, {total_price},
 |          {verified},{canceled},{payment_proof},{shipping_id})
        """.stripMargin)
        .on('user_id -> userId,
          'order_data -> Json.stringify(Json.toJson(orderData)),
          'coupon_id -> couponId,
          'profile -> Json.stringify(Json.toJson(profil)),
          'total_price -> totalPrice, 'verified -> false, 'canceled -> false,
          'payment_proof -> Option.empty[Int], 'shipping_id -> Option.empty[Int])
        .executeInsert(int(1).single)
    }
  }(ec)

  // get all submitted order for a user
  def getUserOrders(userId: Int): Future[Seq[SubmittedOrder]] = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          select * from submitted_order
          where user_id = {user_id}
        """.stripMargin)
        .on('user_id -> userId)
        .as(submittedOrderParser.*)
    }
  }(ec)

  // get all submitted order for a user
  def getOrder(order_id: Int): Future[Option[SubmittedOrder]] = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          select * from submitted_order
          where id = {order_id}
        """.stripMargin)
        .on('order_id -> order_id)
        .as(submittedOrderParser.singleOpt)
    }
  }(ec)

  // get all submitted order
  def getAllOrders(): Future[Seq[SubmittedOrder]] = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          select * from submitted_order
        """.stripMargin)
        .as(submittedOrderParser.*)
    }
  }(ec)

  def verifyOrder(order_id : Int): Future[Int] = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          UPDATE submitted_order
          SET verified = TRUE
          WHERE id = {order_id};
        """.stripMargin)
          .on('order_id -> order_id).executeUpdate()
    }
  }(ec)

  def cancelOrder(order_id : Int): Future[Int] = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          UPDATE submitted_order
          SET canceled = TRUE
          WHERE id = {order_id};
        """.stripMargin)
        .on('order_id -> order_id).executeUpdate()
    }
  }(ec)

  def updateShipmentId(order_id: Int, ship_id: Int) = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          UPDATE submitted_order
          SET shipping_id = {ship_id}
          WHERE id = {order_id}
        """.stripMargin)
        .on('order_id -> order_id, 'ship_id -> ship_id).executeUpdate()
    }
  }(ec)

  def updateProof(order_id: Int, user_id: Int, proof: Int) = Future {
    db.withConnection{ implicit connection =>
      SQL(
        """
          UPDATE submitted_order
          SET payment_proof = {proof}
          WHERE id = {order_id} and user_id = {user_id}
        """.stripMargin)
        .on('order_id -> order_id, 'user_id -> user_id, 'proof -> proof).executeUpdate()
    }
  }(ec)
}