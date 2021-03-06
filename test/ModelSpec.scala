
import org.joda.time.DateTime
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
    import CouponAccessLayer._
    val layer = app.injector.instanceOf(classOf[CouponAccessLayer])

    "Run scenario right" in {
      val dummyCoupon = Coupon(29,0.2,2000,DateTime.now.minusDays(3), DateTime.now().plusDays(54))
      val invalidCoupon = Coupon(32,0.1,3000,DateTime.now.minusDays(3), DateTime.now().minusDays(1))
      val first_part = for {
        _ <- layer.setCoupon(dummyCoupon)
        getCoupon <- layer.getCoupon(dummyCoupon.Id)
        _ <- layer.setCoupon(invalidCoupon)
        getCoupon2 <- layer.getCoupon(invalidCoupon.Id)
        _ <- layer.setCouponUser(dummyCoupon.Id,1,1)
        _ <- layer.setCouponUser(dummyCoupon.Id,2,0)
        _ <- layer.setCouponUser(invalidCoupon.Id,1,2)
      } yield {
        getCoupon.mustEqual(Some(dummyCoupon))
        getCoupon2.mustEqual(Some(invalidCoupon))
      }
      val second_part = for {
        _ <- first_part
        amountOne <- layer.getCouponUserAmount(dummyCoupon.Id,1)
        amount2 <- layer.getCouponUserAmount(dummyCoupon.Id,2)
        amount3 <- layer.getCouponUserAmount(invalidCoupon.Id,1)
        check1 <- layer.checkCoupon(dummyCoupon.Id,1)
        check2 <- layer.checkCoupon(dummyCoupon.Id,2)
        check3 <- layer.checkCoupon(invalidCoupon.Id,1)
        check4 <- layer.checkCoupon(9839,1)
      } yield {
        Int.box(amountOne).mustEqual(1)
        Int.box(amount2).mustEqual(0)
        Int.box(amount3).mustEqual(2)
        check1.mustEqual(Right(dummyCoupon))
        check2.mustEqual(Left(UserDoNotHaveCoupon))
        check3.mustEqual(Left(CouponNotValid))
        check4.mustEqual(Left(CouponNotFound))
      }
      val third_part = for {
        _ <- second_part
        _ <- layer.useCoupon(dummyCoupon.Id,1)
        amount1 <- layer.getCouponUserAmount(dummyCoupon.Id,1)
        _ <- layer.cancelUseCoupon(dummyCoupon.Id,1)
        amount2 <- layer.getCouponUserAmount(dummyCoupon.Id,1)

      } yield {
        Int.box(amount1).mustEqual(0)
        Int.box(amount2).mustEqual(1)
      }
      whenReady(third_part) {_ => None}
    }
  }

  "ProductAccessLayer" should {
    import ProductAccessLayer._
    val layer = app.injector.instanceOf(classOf[ProductAccessLayer])
    "Run scenario right" in {
      val dummyProduct = Product(1,100,9000)
      val productAmounts1 = Seq(ProductAmount(1,10))
      val productAmounts2 = Seq(ProductAmount(1,100))
      val scenario = for {
        _ <- layer.setProduct(dummyProduct)
        products1 <- layer.getProducts(Seq(dummyProduct.ProductId))
        check1 <- layer.checkProducts(productAmounts1)
        _ <- layer.commitProducts(productAmounts1)
        products2 <- layer.getProducts(Seq(dummyProduct.ProductId))
        check2 <- layer.checkProducts(productAmounts2)
        _ <- layer.uncommitProducts(productAmounts1)
        products3 <- layer.getProducts(Seq(dummyProduct.ProductId))
      } yield {
        products1.length.mustEqual(1)
        products1(0).mustEqual(dummyProduct)
        check1.mustEqual(true)
        products2.length.mustEqual(1)
        products2(0).mustEqual(dummyProduct.copy(Amount = 90))
        check2.mustEqual(false)
        products3.length.mustEqual(1)
        products3(0).mustEqual(dummyProduct)
      }
      whenReady(scenario) {_ => None}
    }
  }

  "SubmittedOrderAccessLayer" should {
    val layer = app.injector.instanceOf(classOf[SubmittedOrderAccessLayer])
    val user1 = 1
    val user2 = 2
    val prods1 = List(ProductAmount(1,10),ProductAmount(3,2))
    val prods2 = List(ProductAmount(7,2),ProductAmount(8,8))
    val cid1 = Option.empty[Int]
    val cid2 = Option(3)
    val prof1 = OrderProfile("asdsad","08273123","sadsad@asd.com","JL kalaksksasd")
    val prof2 = OrderProfile("bsasdsad","08472123","asdijwds@asd.com","JL askldjlkasd")
    val total1 = 10000.3
    val total2 = 10222.2
    val shipid1 = 30
    val shipid2 = 93
    val proof1 = 86713
    val proof2 = 93901
    "Run scenario right" in {
      val scenario = for {
        sid1 <- layer.addNewOrder(user1,prods1,cid1,prof1,total1)
        sid2 <- layer.addNewOrder(user2,prods2,cid2,prof2,total2)
        sid3 <- layer.addNewOrder(user1,prods2,cid2,prof1,total2)
        sou1 <- layer.getUserOrders(user1)
        sou2 <- layer.getUserOrders(user2)
        allo <- layer.getAllOrders()
        _ <- layer.cancelOrder(sid3)
        co3 <- layer.getOrder(sid3)
        _ <- layer.updateShipmentId(sid1,shipid1)
        _ <- layer.updateShipmentId(sid2,shipid2)
        _ <- layer.updateProof(sid1,user1,proof1)
        _ <- layer.updateProof(sid2,user2,proof2)
        _ <- layer.verifyOrder(sid1)
        _ <- layer.verifyOrder(sid2)
        vo1 <- layer.getOrder(sid1)
        vo2 <- layer.getOrder(sid2)
      } yield (sid1,sid2,sid3,sou1,sou2,allo,co3,vo1,vo2)
      whenReady(scenario) { _ match {
        case (sid1,sid2,sid3,sou1,sou2,allo,co3,vo1,vo2) => {
          sou1.length mustEqual(2)
          sou2.length mustEqual(1)
          val so1 = sou1.filter(so => so.Id == sid1)(0)
          val so2 = sou2.filter(so => so.Id == sid2)(0)
          val so3 = sou1.filter(so => so.Id == sid3)(0)
          so1.OrderData.mustEqual(prods1)
          so2.OrderData.mustEqual(prods2)
          so3.OrderData.mustEqual(prods2)
          so1.CouponId mustEqual(cid1)
          so2.CouponId mustEqual(cid2)
          so3.CouponId mustEqual(cid2)
          so1.Profil mustEqual(prof1)
          so2.Profil mustEqual(prof2)
          so3.Profil mustEqual(prof1)
          so1.TotalPrice mustEqual(total1)
          so2.TotalPrice mustEqual(total2)
          so3.TotalPrice mustEqual(total2)

          def checknewso(so: SubmittedOrder): Unit = {
            so.Verified mustEqual(false)
            so.Canceled mustEqual(false)
            so.ShippingId mustEqual(Option.empty[Int])
          }
          checknewso(so1)
          checknewso(so2)
          checknewso(so3)
          allo.length mustEqual(3)
          allo.exists(so => so.Id == sid1)
          allo.exists(so => so.Id == sid2)
          allo.exists(so => so.Id == sid3)
          co3.get.Canceled mustEqual(true)
          co3.get.Verified mustEqual(false)
          vo1.get.mustEqual(so1.copy(ShippingId = Some(shipid1), Verified = true, PaymentProof = Some(proof1)))
          vo2.get.mustEqual(so2.copy(ShippingId = Some(shipid2), Verified = true, PaymentProof = Some(proof2)))
        }
      }
      }
    }
  }
}
