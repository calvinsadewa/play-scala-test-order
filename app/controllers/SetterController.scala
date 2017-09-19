package controllers

import javax.inject._

import models._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.ExecutionContext

@Singleton
class SetterController @Inject()(cc: ControllerComponents,
                                 prod_lyr: ProductAccessLayer,
                                 coup_lyr: CouponAccessLayer)
                                (implicit ec : ExecutionContext) extends AbstractController(cc) {
  import play.api.libs.json._
  implicit val datetimeReader = Reads.DefaultJodaDateReads
  implicit val productReader = Json.reads[ProductAccessLayer.Product]
  implicit val couponReader = Json.reads[CouponAccessLayer.Coupon]

  def setProduct() = Action(parse.json[ProductAccessLayer.Product](productReader)).async(req =>
    prod_lyr.setProduct(req.body).map(_ => Ok))

  def setCoupon() = Action(parse.json[CouponAccessLayer.Coupon](couponReader)).async(req =>
    coup_lyr.setCoupon(req.body).map(_ => Ok))

  def setCouponUser(cid: Int, uid: Int, amount: Int) = Action.async(req =>
    coup_lyr.setCouponUser(cid,uid,amount).map(_ => Ok))

}
