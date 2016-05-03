import javax.inject.Inject

import play.api.http._
import play.api.mvc._
import play.api.routing.Router

class RequestHandler @Inject() (router: Router, errorHandler: HttpErrorHandler,
  configuration: HttpConfiguration, filters: HttpFilters) extends DefaultHttpRequestHandler(router, errorHandler, configuration, filters) {

  override def routeRequest(request: RequestHeader) = {
    val reqPath = request.path
    if(
      reqPath == "/" ||
      reqPath.startsWith("/scholar") ||
      reqPath.startsWith("/intl/") ||
      reqPath.startsWith("/images/") ||
      reqPath.startsWith("/favicon.ico") ||
      reqPath.startsWith("/favicon-png.ico") ||
      reqPath.startsWith("/robots.txt")
    ){
      //println("Pass: " + request.path)
      super.routeRequest(request)
    } else {
      println("Intercept: " + request.path)
      Some(Action(Results.NoContent))
    }


  }
}
