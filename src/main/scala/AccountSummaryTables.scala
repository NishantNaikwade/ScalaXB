/**
 * Created by Nishant on 12/8/2015.
 */

import slick.backend.StaticDatabaseConfig
import slick.dbio.DBIO
import slick.dbio.Effect.Write
import slick.lifted.Tag
import slick.jdbc.JdbcBackend.Database
import slick.lifted.{ProvenShape, ForeignKeyQuery}
import slick.driver.MySQLDriver.api._
import slick.profile.FixedSqlAction
import spray.json.DefaultJsonProtocol

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future,ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

object AccountSummaryTables {

}
class CustomerTableOperations{

}
object SampleSlickApplication {

  println("Creating db configuration...")
  val db = Database.forConfig("db")

  //creating TableQuery Object to perform database operations
  val customers: TableQuery[CustomerTable] = TableQuery[CustomerTable]


  def insertCustomer(customer:Customer)={
    val insertAction: DBIO[Int] =
    customers returning customers.map(_.id) += customer
    val f: Future[Int] = db.run(insertAction)
    f.mapTo[Int].map({ id => println("inserted : " + id)})
    Await.result(f, Duration.Inf)
  }

  def updateCustomer(customer: Customer) = {
    val q = customers.filter(_.id === customer.id)
    val updateAction: DBIO[Int] = q.update(customer)
    println("Query : "+ q.updateStatement)
    val f: Future[Int] = db.run(updateAction)
    f.mapTo[Int].map(count => println("Updated row : " + count))
    println("Waiting for Result...")
  }

  def deleteCustomer(id:Int) ={
    val q = customers.filter(_.id === id)
    val deleteAction: DBIO[Int] = q.delete
    val f: Future[Int] = db.run(deleteAction)
    println("Waiting for Result...")
    Await.result(f, Duration.Inf)
    f.onComplete(id =>{
      println("Completed delete operation")
      println("Deleted row id = " + id)
    }
    )
  }

  def getAllCustomers: Future[Seq[Customer]] = {
    println("Inside getAllCustomers method")
    val getAllCustomersQuery = for (c <- customers) yield c
    val getAllCustomersAction = getAllCustomersQuery.result
    println("Before Database execution")
    val getAllCustomersFuture: Future[Seq[Customer]] = db.run(getAllCustomersAction)
    println("Databse call executed...")
    getAllCustomersFuture
  }

  def getCustomerById(id:Int):Future[Customer]={
    val customerByIdQuery = customers.filter(customer => customer.id === id)
    val customerByIdAction = customerByIdQuery.result
    val customerByIdFuture: Future[Customer]  = db.run(customerByIdAction).map(_.head)
    customerByIdFuture
  }
}
case class Customer(id:Option[Int],fname:String,lname:String,mobnumber:String,email:String)
case class CustomerList(customers: Seq[Customer])

object CustomerListProtocol extends DefaultJsonProtocol {
  import MyJsonProtocol._
  implicit val CustomerListFormat = jsonFormat1(CustomerList.apply)
}
class CustomerTable(tag:Tag)extends Table[Customer](tag,"customer"){
  def id = column[Int]("id",O.PrimaryKey,O.AutoInc)
  def fname = column[String]("fname")
  def lname = column[String]("lname")
  def mobnumber = column[String]("mobnumber")
  def email = column[String]("email")


//  override def * = (id, fname, lname, mobnumber, email)
def * = (id.?, fname,lname,mobnumber,email) <> (Customer.tupled, Customer.unapply _)
}
