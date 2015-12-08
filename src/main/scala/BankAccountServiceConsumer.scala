import akka.actor.{Props, Actor}
import generated._

import scala.concurrent.{Await, ExecutionContext, Future}
import scalaxb._
import scalaxb.Soap11ClientsAsync
import scala.concurrent.duration._

object BankAccountServiceInvoker{

  def getAccountSummary : String = {
    val remote = new AccountSummarySoapBindings with Soap11ClientsAsync with DispatchHttpClientsAsync{}
    var accountStr:String = "Hello"
    val response:Future[Account] = remote.service.getAccountSummary(Some("nnaiwkad@in.ibm.com"),Some("Nishant"),Some("Naikwade"),Some("9422926999"))

    response.onComplete(account=>{
      println("Inside onComplete")
      accountStr = MyJsonProtocol.AccountSummaryResponseFormat.write(AccountSummaryResponse(account.get.accountId.get,account.get.accountType.get,account.get.balance.get)).toString()
      println(accountStr)
    })
    while(!response.isCompleted){
      Thread.sleep(1000)
    }

   accountStr
  }


  def getAccountFuture : Future[Account] = {
    val remote = new AccountSummarySoapBindings with Soap11ClientsAsync with DispatchHttpClientsAsync{}

    val response:Future[Account] = remote.service.getAccountSummary(Some("nnaiwkad@in.ibm.com"),Some("Nishant"),Some("Naikwade"),Some("9422926999"))

   response
  }
}

class BankAccountActor extends Actor{
   def receive : Receive = {
     case "AccountSummary" =>{
       println("Calling Account Summary")
       val actorRef = context.actorOf(Props(new AccountHttpService(null)),"AccountService")

       var accountSummary:String = BankAccountServiceInvoker.getAccountSummary

       sender ! "AccountSummaryResponse"
     }
  }
}