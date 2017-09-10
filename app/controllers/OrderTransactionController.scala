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
    val addProduct = checkAuth.right.map { _ =>
      val productAmount = ProductAmount(request.body.product_id,request.body.quantity)
      brow_lyr.addProductToOrder(request.body.user_id,productAmount,request.body.version)
    }
    val checkError = addProduct.right.map{f => f.map ( _ match {
      case Left(err_code) => Results.BadRequest(err_code.toString())
      case Right(version) => Results.Ok(version.toString())
    })}
    val ret = checkError

    ret match {
      case Left(error) => Future(error)
      case Right(result) => result
    }
  }

  def deleteBrowse(userId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }

  def applyCoupon(userId: Int,couponId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }

  def updateProfile(userId: Int) = Action(parse.json[OrderProfile]).async { request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }

  def submit(userId: Int) = Action.async {request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }
}