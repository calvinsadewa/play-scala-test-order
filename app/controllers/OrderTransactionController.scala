package controllers

import javax.inject._

import models.{OrderInBrowseAccessLayer, OrderProfile, ProductAmount}
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class AddProductData(user_id: Int,product_id: Int,quantity: Int,version: Int)

object  OrderTransactionController {
  val API_KEY = "API_KEY"
  val CUSTOMER_API_KEY = "Magi3iewdc8icasd"
  val ADMIN_API_KEY = "Masadnuewjd923gi3iewdc8icasd"

  object JsonConverter {
    implicit val addProductDataReader = Json.reads[AddProductData]
    implicit val addProductDataWriter = Json.writes[AddProductData]
  }

  def CheckCustomerAuth[A](request: Request[A]) = {
    if (request.getQueryString(API_KEY).getOrElse("") == CUSTOMER_API_KEY) {
      Right(None)
    }
    else {
      Left(Results.Unauthorized)
    }
  }

  def CheckAdminAuth[A](request: Request[A]) = {
    if (request.getQueryString(API_KEY).getOrElse("") == ADMIN_API_KEY) {
      Right(None)
    }
    else {
      Left(Results.Unauthorized)
    }
  }
}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class OrderTransactionController @Inject()(cc: ControllerComponents, brow_lyr: OrderInBrowseAccessLayer)(implicit ec : ExecutionContext) extends AbstractController(cc) {
  import models.OrderJsonConverter._
  import OrderTransactionController.JsonConverter._
  import extensions.NiceFutureEither._
  def getBrowse(userId: Int) = Action.async { request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val ret = checkAuth.right.map(_ => brow_lyr.getByUserId(userId).map(x => Ok(Json.toJson(x))))

    ret match {
      case Left(error) => Future(error)
      case Right(result) => result
    }
  }

  def addProduct = Action(parse.json[AddProductData]).async { request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val addProduct = Future(checkAuth).map (_.right.map { _ =>
      val productAmount = ProductAmount(request.body.product_id,request.body.quantity)
      brow_lyr.addProductToOrder(request.body.user_id,productAmount,request.body.version).map(_ match {
        case Left(x) => Left(Results.BadRequest(x.toString))
        case Right(version) => Right(version)
      })
    }.toFuture).flatten.map(_.right.flatMap(x => x))

    addProduct map {
      case Left(error) => error
      case Right(version) => Results.Ok(version.toString)
    }
  }

  def deleteBrowse(userId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => x.map(y => Ok)
    }
  }

  def applyCoupon(userId: Int,couponId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val applyCouponAct = checkAuth.right.map(_ => brow_lyr.applyCouponToOrder(userId,couponId))
    applyCouponAct match {
      case Left(error) => Future(error)
      case Right(x) => x.map(y => Ok)
    }
  }

  def updateProfile(userId: Int) = Action(parse.json[OrderProfile]).async { request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val updateProfileAct = checkAuth.right.map(_ => brow_lyr.changeProfile(userId,request.body))
    updateProfileAct match {
      case Left(error) => Future(error)
      case Right(x) => x.map(y => Ok)
    }
  }

  def submit(userId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    // Check current order in browse
    // Check Product and Coupon
    // Commit product and coupon
    // Create new submittedorder
    // return submitted order
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }
}