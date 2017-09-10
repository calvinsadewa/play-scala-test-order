package controllers

import javax.inject._

import models._
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
class OrderTransactionController @Inject()(cc: ControllerComponents,
                                           brow_lyr: OrderInBrowseAccessLayer,
                                           prod_lyr: ProductAccessLayer,
                                           coup_lyr: CouponAccessLayer)
                                          (implicit ec : ExecutionContext) extends AbstractController(cc) {
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
    val getCurrentOrder = Future(checkAuth).map(_.right.map( _ =>
      brow_lyr.getByUserId(userId)
    ).toFuture).flatten
    val checkCurrentOrder = getCurrentOrder.map(_.right.flatMap { ord =>
      if (ord.Profil.isEmpty)
        Left(Results.Forbidden)
      else
        Right(ord)
    })
    val checkStock = checkCurrentOrder.map(_.right.map(o =>
      prod_lyr.checkProducts(o.OrderData).map(_ match {
        case true => Right(o)
        case false => Left(Results.Conflict("Product:Some is empty"))
      })).toFuture).flatten.map(_.right.flatMap(x => x))
    val checkCoupon = checkStock.map(_.right.map(o => o.CouponId.map(cid =>
      coup_lyr.checkCoupon(cid,userId).map(_ match {
        case Left(error) => Left(Results.BadRequest("Coupon:" + error.toString))
        case Right(coupon) => Right((o,Some(coupon)))
      })
    ).getOrElse(Future(Right((o,None))))).toFuture.map(_.right.flatMap(x => x))).flatten
    val submitOrder = checkCoupon.map(_.right.map{ tup =>
      val (order,optCoup) = tup
      val getProducts = prod_lyr.getProducts(order.OrderData.map(s => s.ProductId))
      val calculatePrice = getProducts.map { products =>
        val pid2price = products.map(s => s.ProductId -> s.PricePer).toMap
        val initial_price = order.OrderData.map(d => {
          pid2price(d.ProductId) * d.Amount
        }).sum
        val discount_price = optCoup.map(c => initial_price * (1-c.PortionCut) - c.NominalCut).getOrElse(initial_price)
        (discount_price,order)
      }
      for {(price, order) <- calculatePrice
           _ <- prod_lyr.commitProducts(order.OrderData)
           _ <- order.CouponId.map(cid => coup_lyr.useCoupon(cid, order.UserId)).getOrElse(Future(0))
      } yield 1

      false
    })
    // Check current order in browse
    // Check Product and Coupon
    // Create new submittedorder and product and coupon
    // return submitted order
    val deleteAct = checkAuth.right.map(_ => brow_lyr.delete(userId))
    deleteAct match {
      case Left(error) => Future(error)
      case Right(x) => Future(Ok)
    }
  }
}