/**
 * Created by Nishant on 12/8/2015.
 */

import slick.dbio.DBIO
import slick.lifted.Tag
import slick.jdbc.JdbcBackend.Database
import slick.lifted.{ProvenShape, ForeignKeyQuery}
import slick.driver.MySQLDriver.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object AccountSummaryTables {

}

object SampleSlickApplication extends App {

  println("Creating db configuration...")
  val db = Database.forConfig("db")
  val customers: TableQuery[CustomerTable] = TableQuery[CustomerTable]

  val setupAction: DBIO[Unit] = DBIO.seq(
    // Create the schema by combining the DDLs for the Suppliers and Coffees
    // tables using the query interfaces

    // Insert some suppliers
    customers += (0 , "Nishant", "Naikwade", "9422926999", "nnaikwad@in.ibm.com")
  )
  val setupFuture: Future[Unit] = db.run(setupAction)

  Await.result(setupFuture, Duration.Inf)
}
case class Customer(fname:String,lname:String,mobnumber:String,email:String)
//type Customer(Int,String,String,String,String)
class CustomerTable(tag:Tag)extends Table[(Int,String,String,String,String)](tag,"customer"){
  def id = column[Int]("id",O.PrimaryKey,O.AutoInc)
  def fname = column[String]("fname")
  def lname = column[String]("lname")
  def mobnumber = column[String]("mobnumber")
  def email = column[String]("email")


  override def * = (id, fname, lname, mobnumber, email)
}