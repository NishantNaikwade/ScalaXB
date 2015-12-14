import akka.actor._
import generated.Account
import spray.can.Http
import spray.http.HttpHeaders.RawHeader
import spray.http._
import spray.httpx.marshalling.{ToResponseMarshaller, Marshaller}
import spray.json.{JsValue, RootJsonFormat, DefaultJsonProtocol}
import spray.routing._
import spray.routing.directives.{RouteDirectives}
import spray.util.LoggingContext
import spray.http.StatusCodes._
import scala.concurrent.{Await, Future, ExecutionContext}
import util.control.NonFatal
import spray.http.HttpMethods.GET

/**
 * Created by Nishant on 10/30/2015.
 */

import akka.io.IO
import scala.concurrent.duration._

object StartAccountSummary extends App {
  //Create an Actor System
  var system = ActorSystem("Account-Service-Actor")

  //Create an ActorRef of AccountServiceSample for Test purpose
  val accountServiceSampleActor = system.actorOf(Props(new AccountServiceSample))

  private implicit val _ = system.dispatcher
  //Create routes for Rest Service
  val accountServiceRoutes = new AccountServiceDirective().route
  //Create an ActorRef to the HttpService and Actor
  val accountService = system.actorOf(Props(new AccountHttpService(accountServiceRoutes)),name="AccountService")
  //  val accountService = system.actorOf(Props(new AccountServiceSample))

  //Create a Http Server
  IO(Http)(system) ! Http.Bind(accountService, "0.0.0.0", port = 8181)
}

class AccountServiceSample extends Actor {
  def receive = {
    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      sender ! HttpResponse(entity = "pong")
  }
}


class AccountServiceDirective(implicit executionContext: ExecutionContext) extends Directives {


  val route = {
    respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*")) {
      path("sample") {
        get {
          respondWithMediaType(MediaTypes.`text/html`) {
            // XML is marshalled to `text/xml` by default, so we simply override here
            complete {
              <html>
                <body>
                  <h1>Say hello to
                    <i>spray-routing</i>
                    on
                    <i>spray-can</i>
                    !</h1>
                </body>
              </html>
            }
          }
        }
      } ~
        path("getAccountSummary") {
          get {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete {
                BankAccountServiceInvoker.getAccountSummary.mapTo[generated.Account].map(account => {
                  println("Inside map block expression")
                  println(account)
                  MyJsonProtocol.AccountSummaryResponseFormat.write(AccountSummaryResponse(account.accountId.get,account.accountType.get,account.balance.get)).toString()
                })
              }
            }
          }
        } ~
        path("getAllCustomers") {
          get {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete {
                println("Inside getAllCustomers")
                SampleSlickApplication.getAllCustomers.mapTo[Seq[Customer]].map(customerList => {
                  CustomerListProtocol.CustomerListFormat.write(CustomerList(customerList.toList)).asJsObject.toString()
                })
              }
            }
          }
        } ~
        path("getAccountBalance") {
          get {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete {

                println("Inside get account balance")
                val accountSummary = AccountSummary("12345", "Nishant", 26)

                val response = MyJsonProtocol.AccountSummaryFormat.write(accountSummary)

                HttpResponse(entity = response.toString())
              }
            }
          }
        } ~
        path("checkAccountInfo") {
          post {
            import MyJsonProtocol._
            import spray.httpx.SprayJsonSupport.sprayJsonUnmarshaller
            entity(as[AccountSummary]) { accountSummary =>



              println(accountSummary)
              respondWithMediaType(MediaTypes.`application/json`) {
                complete {

                  println("Inside get account balance")
                  val validAccountSummary = ValidAccountSummary("Account Summary is valid")

                  val response = MyJsonProtocol.ValidAccountSummaryFormat.write(validAccountSummary)

                  HttpResponse(entity = response.toString())
                }
              }
            }
          }
        } ~
        path("getCustomerById" / IntNumber) { (id) => {
          get {
            complete {
              println("Customer id is : " + id)
              SampleSlickApplication.getCustomerById(id).mapTo[Customer].map(customer => {
                MyJsonProtocol.CustomerResponseFormat.write(customer).asJsObject.toString()
              })
            }
          }
        }
        }
    }
  }
  }

  case class AccountSummary(accountNumber: String, firstName: String, age: Int)

  case class AccountSummaryResponse(accountId: String, accountType: String, balance: String)

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit val AccountSummaryFormat = jsonFormat3(AccountSummary)
    implicit val ValidAccountSummaryFormat = jsonFormat1(ValidAccountSummary)
    implicit val AccountFormat = jsonFormat3(Account)
    implicit val AccountSummaryResponseFormat = jsonFormat3(AccountSummaryResponse)
    implicit val AccountResponseFormat = jsonFormat3(generated.Account)
    implicit val CustomerResponseFormat = jsonFormat5(Customer)


  }

  case class ValidAccountSummary(message: String)


  case class ErrorResponseException(responseStatus: StatusCode, response: Option[HttpEntity]) extends Exception

  class AccountHttpService(route: Route) extends Actor with HttpService with ActorLogging {

    implicit def actorRefFactory = context

    implicit val handler = ExceptionHandler {
      case NonFatal(ErrorResponseException(statusCode, entity)) => ctx =>
        ctx.complete(statusCode, entity)

      case NonFatal(e) => ctx => {
        log.error(e, InternalServerError.defaultMessage)
        ctx.complete(InternalServerError)
      }
    }


    def receive: Receive = {



      runRoute(route)(handler, RejectionHandler.Default, context, RoutingSettings.default, LoggingContext.fromActorRefFactory)


    }
  }

  //trait CrossLocationRouteDirectives extends RouteDirectives {
  //
  //  implicit def fromObjectCross[T : Marshaller](origin: String)(obj: T) =
  //    new ToResponseMarshaller[T] {
  //      def route: StandardRoute = new CompletionRoute(OK,
  //        RawHeader("Access-Control-Allow-Origin", origin) :: Nil, obj)
  //    }
  //
  //  private class CompletionRoute[T : Marshaller](status: StatusCode, headers: List[HttpHeader], obj: T)
  //    extends StandardRoute {
  //    def apply(ctx: RequestContext): Unit = {
  //      ctx.complete(status, headers, obj)
  //    }
  //  }
  //}

  trait FailureHandling {
    this: HttpService =>

    // For Spray > 1.1-M7 use routeRouteResponse
    // see https://groups.google.com/d/topic/spray-user/zA_KR4OBs1I/discussion
    def rejectionHandler: RejectionHandler = RejectionHandler.Default

    def exceptionHandler(implicit log: LoggingContext) = ExceptionHandler {

      case e: IllegalArgumentException => ctx =>
        loggedFailureResponse(ctx, e,
          message = "The server was asked a question that didn't make sense: " + e.getMessage,
          error = NotAcceptable)

      case e: NoSuchElementException => ctx =>
        loggedFailureResponse(ctx, e,
          message = "The server is missing some information. Try again in a few moments.",
          error = NotFound)

      case t: Throwable => ctx =>
        // note that toString here may expose information and cause a security leak, so don't do it.
        loggedFailureResponse(ctx, t)
    }

    private def loggedFailureResponse(ctx: RequestContext,
                                      thrown: Throwable,
                                      message: String = "The server is having problems.",
                                      error: StatusCode = InternalServerError)
                                     (implicit log: LoggingContext): Unit = {
      log.error(thrown, ctx.request.toString)
      ctx.complete(error, message)
    }

  }
