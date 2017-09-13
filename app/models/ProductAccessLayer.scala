package models
import javax.inject.Inject

import akka.event.slf4j.Logger
import anorm.SqlParser._
import anorm._
import play.api.db.DBApi

import scala.concurrent.Future

@javax.inject.Singleton
class ProductAccessLayer @Inject()(dbapi: DBApi)(implicit ec: DatabaseExecutionContext){
  private val db = dbapi.database("default")

  case class Product(ProductId:Int, Amount: Int, PricePer: Double)

  private val productParser = {
    get[Int]("id") ~
      get[Int]("amount") ~
      get[Double]("price_per")map {
      case id~amount~price_per=>
        Product(id, amount, price_per)
    }
  }

  // check whether product stock exist
  def checkProducts(productAmounts: Seq[ProductAmount]) = Future {
    db.withTransaction { implicit connection =>
      productAmounts.forall { p =>
        val amount = SQL("SELECT amount FROM products WHERE id={product_id}").on('product_id -> p.ProductId).as(scalar[Long].singleOpt)
        amount.map(a => a >= p.Amount).getOrElse(false)
      }
    }
  }(ec)

  // commit products stock
  def commitProducts(productAmounts: Seq[ProductAmount]) = Future {
    db.withTransaction { implicit connection =>
      productAmounts.foreach { p =>
        val amount = SQL("SELECT amount FROM products WHERE id={product_id}").on('product_id -> p.ProductId).as(scalar[Long].single)
        SQL("UPDATE products SET amount = {new_amount} WHERE id={product_id}").on('product_id -> p.ProductId, 'new_amount -> (amount - p.Amount))
      }
    }
  }(ec)

  def uncommitProducts(productAmounts: Seq[ProductAmount]) = Future {
    db.withTransaction { implicit connection =>
      productAmounts.foreach { p =>
        val amount = SQL("SELECT amount FROM products WHERE id={product_id}").on('product_id -> p.ProductId).as(scalar[Long].single)
        SQL("UPDATE products SET amount = {new_amount} WHERE id={product_id}").on('product_id -> p.ProductId, 'new_amount -> (amount + p.Amount))
      }
    }
  }(ec)

  // get products
  def getProducts(productIds: Seq[Int]) = Future {
    db.withTransaction { implicit connection =>
      SQL("SELECT (id,amount,price_per) FROM products WHERE id in ({product_ids})")
        .on('product_ids -> productIds).as(productParser.*)
    }
  }(ec)

  // set product
  def setProduct(product: Product) = Future {
    db.withTransaction { implicit connenction =>
      SQL("DELETE FROM products WHERE id={product_id}").on('product_id -> product.ProductId).execute()
      SQL("INSERT FROM products (id,amount,price_per) VALUES ({pid}, {amount}, {price_per})")
        .on('pid -> product.ProductId,
          'amount -> product.Amount,
          'price_per -> product.PricePer).execute()
    }
  }(ec)
}