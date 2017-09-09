
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

  def computerService: ComputerRepository = app.injector.instanceOf(classOf[ComputerRepository])

  "Computer model" should {

    "be retrieved by id" in {
      whenReady(computerService.findById(21)) { maybeComputer =>
        val macintosh = maybeComputer.get

        macintosh.name must equal("Macintosh")
        macintosh.introduced.value must matchPattern {
          case date:java.util.Date if dateIs(date, "1984-01-24") =>
        }
      }
    }
    
    "be listed along its companies" in {
        whenReady(computerService.list()) { computers =>

          computers.total must equal(574)
          computers.items must have length(10)
        }
    }
    
    "be updated if needed" in {

      val result = computerService.findById(21).flatMap { computer =>
        computerService.update(21, Computer(name="The Macintosh",
          introduced=None,
          discontinued=None,
          companyId=Some(1))).flatMap { _ =>
          computerService.findById(21)
        }
      }

      whenReady(result) { computer =>
        val macintosh = computer.get

        macintosh.name must equal("The Macintosh")
        macintosh.introduced mustBe None
      }
    }
  }

  "OrderInBrowseAccessLayer" should {
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
  }
}
