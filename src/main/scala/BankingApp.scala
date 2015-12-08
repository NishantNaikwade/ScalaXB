package myapp

import akka.actor.{PoisonPill, Props, Actor, ActorSystem}
import akka.event.Logging

import scala.util.Random

/**
 * Created by Nishant on 12/3/2015.
 */



case class Customer(id: Int, requestType:String)
object BankBranch{
  def props : Props = Props(new BankBranch)
}


object Hello
case class DepositForm(customer:Customer,amount:Int)
case class WithdrawForm(customer:Customer,amount:Int)
case class Request()
case class ApplicationForm(customer:Customer)

object Cashier{
  def props:Props = Props(new Cashier)
}
class Cashier extends Actor with akka.actor.ActorLogging{

  override val log = Logging(context.system, this)

  override def preStart = {
    log.info("prestart of Cashier Path " + self.path)
  }
  def receive : Receive = {
    case Hello => {
      log.info("Hello Message Received")
      println("Welcome Sir! How may I help you?")
      sender() ! Request
    }
    case DepositForm(customer,amount) => {
      log.info("Sure Sir! Please wait while we deposit amount...")
      Thread.sleep(Random.nextInt(5) * 1000)
      log.info("We have deposited the amount " + amount + " to AccountNumber " + BankingApp.accountIdPrefix + customer.id)
      log.info("Thank you!")

    }
    case WithdrawForm(customer,amount) => {
      log.info("Sure Sir! Please wait while we withdraw amount...")
      Thread.sleep(Random.nextInt(5) * 1000)
      log.info("Please collect the amount " + amount)
      log.info("Thank you!")

    }
    case _ => {
      log.info("Sir I could not understand your request")
      log.info("We are transferring your request to Help Desk")
      //context.actorOf(HelpDesk.props) ! _
    }
  }


}
object BankCustomer{
  def props(requestType : String): Props = Props(new BankCustomer(requestType))
}
class BankCustomer(requestType : String) extends Actor with akka.actor.ActorLogging{
  override val log = Logging(context.system, this)

  def receive : Receive = {
    case Request => {
      log.info("Yes. My request is related to " + requestType)
      context.actorOf(Cashier.props) ! requestType
    }
    case _ => {
      log.info("I am unable to understand what I need to provide")

    }
  }
}



case class NewAccount(customer:Customer)
object BankAssociate{
  def props: Props = Props(new BankAssociate)
}

class BankAssociate extends Actor with akka.actor.ActorLogging{
  override val log = Logging(context.system, this)

  override def preStart = {
    log.info("prestart of BankAssociate Path " + self.path)
  }

  def receive : Receive = {
    case NewAccount(customer) => {
      log.info("Thank you for doing business with us.")
      log.info("Your account number is "+ BankingApp.accountIdPrefix + customer.id)

    }
    case _ => {
      log.info("Sorry for inconvenience. We are unable to process your request")
      log.info("We are transferring your request to Help Desk")
      //context.actorOf(HelpDesk.props) ! _

    }
  }
}

object HelpDesk{
  def props: Props = Props(new HelpDesk)
}
class HelpDesk extends Actor with akka.actor.ActorLogging{
  override val log = Logging(context.system, this)

  override def preStart = {
    log.info("prestart of HelpDesk Path " + self.path)
  }

  def receive : Receive = {
    case Customer(id,requestType) => {
      log.info("Please fill up the form for " + requestType + " Service")
      requestType match {
        case "Open Account" => {
          context.actorOf(BankAssociate.props,"BankAssociate") ! NewAccount(new Customer(id,requestType))
        }
        case "Deposit" => {
          context.actorOf(Cashier.props,"Cashier") ! DepositForm(Customer(id,requestType),Random.nextInt(500))
        }
        case "Withdraw" => {
          context.actorOf(Cashier.props,"Cashier") ! DepositForm(Customer(id,requestType),Random.nextInt(500))
        }
      }
      sender ! requestType
    }
    case _ => {
      println("Sorry for inconvenience. We are unable to process your request")

    }
  }
}


class BankBranch extends Actor{

   val log = Logging(context.system, this)



  def receive : Receive = {
    case Customer(id,requestType) => {
      log.info("Welcome Sir! Please give us a moment to provide you assistance")
      log.info("Your id is : " + id)
      Thread.sleep(1000)
      Random.nextInt(3) match {
        case 0 => {
          context.actorOf(Cashier.props,"Cashier") ! new Customer(id,requestType)
        }
        case 1 => {
          context.actorOf(Cashier.props,"Cashier") ! new Customer(id,requestType)
        }
        case 2 => {
          context.actorOf(HelpDesk.props,"HelpDesk") ! new Customer(id,requestType)
        }
        case 3 =>{
          context.actorOf(BankAssociate.props,"BankAssociate") ! new Customer(id,requestType)
        }
      }
    }
    case _ => log.info("Unknown request message")
      context.stop(self)
  }
}

object BankingApp extends App{

  val system = start
  val bankBranch = system.actorOf(BankBranch.props,"Shivaji-Nagar-Branch")
  //  val cashier = system.actorOf(Cashier.props)
  //  val customer = system.actorOf(BankCustomer.props("Deposit"))
  val customerCount = 10
  val accountIdPrefix = "91100232000"
  def start: ActorSystem = {
    ActorSystem("Banking-System")
  }
  def stop: Unit = {
    system.terminate()
  }
  //val log = Logging(system, this)
  def openBranch : Unit = {
   println("Path of BankBranch : " + bankBranch.path)
    var currentCusotmerCount = 0
    val requestType = Array("Deposit","Withdraw","Open Account")
    while(customerCount!=currentCusotmerCount){
      currentCusotmerCount+=1
      var index = Random.nextInt(requestType.length)
      println("index : " + index)
      if(index>0)
        index = index -1

      bankBranch ! new Customer(currentCusotmerCount,requestType(index))
    }
  }

  openBranch

}