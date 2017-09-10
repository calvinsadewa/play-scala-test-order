package controllers

import javax.inject._

import models.OrderInBrowseAccessLayer
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class OrderTransactionController @Inject()(cc: ControllerComponents, brow_lyr: OrderInBrowseAccessLayer)(implicit ec : ExecutionContext) extends AbstractController(cc) {
  import models.OrderJsonConverter._
  def getBrowse(userId: Int) = Action.async { request =>
    val checkAuth = OrderTransactionController.CheckCustomerAuth(request)
    val ret = checkAuth.right.map(_ => brow_lyr.getByUserId(userId).map(x => Ok(Json.toJson(x))))

    ret match {
      case Left(error) => Future(error)
      case Right(result) => result
    }
  }
}


case class AddProductData(user_id: Int,product_id: Int,quantity: Int,version: Int)

object  OrderTransactionController {
  val API_KEY = "API_KEY"
  val CUSTOMER_API_KEY = "Magi3iewdc8icasd"
  val ADMIN_API_KEY = "Masadnuewjd923gi3iewdc8icasd"

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
      None
    }
    else {
      Some(Results.Unauthorized)
    }
  }
}