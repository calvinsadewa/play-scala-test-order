
import controllers.OrderTransactionController
import models._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import org.mockito.Mockito._

import scala.concurrent.Future

class FunctionalSpec extends PlaySpec with GuiceOneAppPerSuite with ScalaFutures with MockitoSugar{
  import scala.concurrent.ExecutionContext.Implicits.global

  "OrderTransactionController" should {
    import OrderTransactionController._
    import models.OrderJsonConverter._
    val mockorderbrowseacl = mock[OrderInBrowseAccessLayer]
    val mockpaacl = mock[ProductAccessLayer]
    val mockcaacl = mock[CouponAccessLayer]
    val mocksoacl = mock[SubmittedOrderAccessLayer]
    val orderTransactionController = new OrderTransactionController(
      stubControllerComponents(),
      mockorderbrowseacl, mockpaacl, mockcaacl,mocksoacl)
    val consumer_request = FakeRequest(GET,s"/bla?API_KEY=$CUSTOMER_API_KEY")
    "deny unauthorized request" in {
      val r1 = orderTransactionController.getBrowse(0)(FakeRequest())
      status(r1) must equal(UNAUTHORIZED)
    }

    "get browsing order" in {
      var dummyOrder = new OrderInBrowse(3,List(),5,Some(1),None)
      when(mockorderbrowseacl.getByUserId(10)).thenReturn(Future(dummyOrder))
      val r1 = orderTransactionController.getBrowse(10)(consumer_request)
      status(r1) must equal(OK)
      contentAsJson(r1).as[OrderInBrowse].mustEqual(dummyOrder)
    }

    "modify browsing order" in {
      var dummyOrder = new OrderInBrowse(3,List(),5,Some(1),None)
      when(mockorderbrowseacl.getByUserId(10)).thenReturn(Future(dummyOrder))
      val r1 = orderTransactionController.getBrowse(10)(consumer_request)
      status(r1) must equal(OK)
      contentAsJson(r1).as[OrderInBrowse].mustEqual(dummyOrder)
    }
  }


}
