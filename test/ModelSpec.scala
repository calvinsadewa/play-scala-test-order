
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatest.time.{Millis, Seconds, Span}

class ModelSpec extends PlaySpec with GuiceOneAppPerSuite with ScalaFutures {
  import models._

  import scala.concurrent.ExecutionContext.Implicits.global

  // -- Date helpers
  
  def dateIs(date: java.util.Date, str: String) = {
    new java.text.SimpleDateFormat("yyyy-MM-dd").format(date) == str
  }

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  
  // --

  "OrderInBrowseAccessLayer" should {
    import OrderInBrowseAccessLayer.ErrorCode._
    val layer = app.injector.instanceOf(classOf[OrderInBrowseAccessLayer])
    "return empty result with new get" in (
      whenReady(layer.getByUserId(8931)){
        order =>
          order.Version must equal(0)
          order.UserId must equal(8931)
          order.OrderData.toList.length must equal(0)
      }
      )

    "Remember Added Product and version" in {
      whenReady(
        layer.addProductToOrder(23, ProductAmount(2, 10), 0).flatMap(x =>
          layer.addProductToOrder(23, ProductAmount(3, 20), 1)).flatMap { x =>
          layer.addProductToOrder(23, ProductAmount(2, 11), 2)
        }.flatMap(x =>
          layer.addProductToOrder(23, ProductAmount(2, 11), 3)).flatMap(x =>
          layer.addProductToOrder(23, ProductAmount(4, 2), 4)).flatMap(x =>
          layer.getByUserId(23))) {
        order =>

          order.Version must equal(5)
          order.UserId must equal(23)
          order.OrderData.toList.length must equal(3)
          val prod2amount = order.OrderData.map(p => (p.ProductId, p.Amount)).toMap
          prod2amount.getOrElse(2, 0) must equal(32)
          prod2amount.getOrElse(3, 0) must equal(20)
          prod2amount.getOrElse(4, 0) must equal(2)
      }
    }

    "Error on below zero product" in {
      whenReady(
        layer.addProductToOrder(231, ProductAmount(2, 10), 0).flatMap(x =>
          layer.addProductToOrder(231, ProductAmount(3, 20), 1)).flatMap { x =>
          layer.addProductToOrder(231, ProductAmount(2, -11), 2)
        }.map(x => x.mustEqual(Left(ProductAmountBelowZero))).flatMap(x =>
          layer.getByUserId(231))) {
        order =>
          order.Version must equal(2)
          order.UserId must equal(231)
          order.OrderData.toList.length must equal(2)
          val prod2amount = order.OrderData.map(p => (p.ProductId, p.Amount)).toMap
          prod2amount.getOrElse(2, 0) must equal(10)
          prod2amount.getOrElse(3, 0) must equal(20)
      }
    }

    "Error on not lastest version" in {
      whenReady(
        layer.addProductToOrder(232, ProductAmount(2, 10), 0).flatMap(x =>
          layer.addProductToOrder(232, ProductAmount(3, 20), 0))
          .map(x => x.mustEqual(Left(NotLastestVersion))).flatMap(x =>
          layer.getByUserId(232))) {
        order =>
          order.Version must equal(1)
          order.UserId must equal(232)
          order.OrderData.toList.length must equal(1)
          val prod2amount = order.OrderData.map(p => (p.ProductId, p.Amount)).toMap
          prod2amount.getOrElse(2, 0) must equal(10)
      }
    }

    "Remember Coupon and Profile" in {
      val profile = OrderProfile("Jas","977290","askjdlsd.test.com","blasblasds")
      whenReady(
        layer.applyCouponToOrder(183,1).flatMap(x =>
        layer.applyCouponToOrder(183,3)).flatMap(x =>
        layer.changeProfile(183,profile)).flatMap(x =>
          layer.getByUserId(183))) {
        order =>
          order.CouponId must equal(Some(3))
          order.Profil mustEqual(Some(profile))
      }
    }
    "Delete" in {
      whenReady(
        layer.addProductToOrder(223, ProductAmount(2, 10), 0).flatMap(x =>
          layer.addProductToOrder(223, ProductAmount(3, 20), 1)).flatMap{x =>
          layer.addProductToOrder(223, ProductAmount(2, 11), 2)}.flatMap(x =>
          layer.addProductToOrder(223, ProductAmount(2, 11), 3)).flatMap(x =>
          layer.addProductToOrder(223, ProductAmount(4, 2), 4)).flatMap(x =>
          layer.delete(223)).flatMap(x =>
          layer.getByUserId(223))) {
        order =>

          order.Version must equal(0)
          order.UserId must equal(223)
          order.OrderData.toList.length must equal(0)
      }
    }
  }

  "CouponAccessLayer" should {
    import CouponAccessLayer.ErrorCode._
    val layer = app.injector.instanceOf(classOf[CouponAccessLayer])

    "Set coupon correctly" in {

    }
  }
}
