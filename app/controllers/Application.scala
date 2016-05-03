package controllers

import java.util.Base64
import akka.stream.Materializer
import com.google.inject.Inject
import play.api.http.HttpEntity
import play.api.libs.ws.{StreamedResponse, WSClient}
import play.api.mvc._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Application @Inject() (ws: WSClient, implicit val mat: Materializer) extends Controller {
  val ignoreHeaders = Set("host", "play_session", "x-request-id", "x-forwarded-for", "x-forwarded-proto", "x-forwarded-port", "via", "connect-time", "x-request-start", "total-route-time")

  /**
    * Proxy all requests to Google Search.
    * @param pathPart match the sub path in request.
    * @param bbaassee raw query parameters encoded in base64 format.
    * @return Result
    */
  def get(pathPart: String, bbaassee: String) = Action.async{ request =>
    request.path match {
      case path =>
        //Remove proxy headers
        var headers = request.headers.toSimpleMap.filterKeys{ key =>
          !ignoreHeaders.contains(key.trim.toLowerCase())
        }
        //Refine referer
        headers = headers.map{
          case (k, v) if k.trim.toLowerCase == "referer" =>
            (k, v.replaceFirst("""//[^/]+/?""", "//scholar.google.com/"))
          case other => other
        }
        val refinedRawQueryStr = bbaassee match {
          case base if base.trim == "" =>
            request.rawQueryString
          case base =>
            val bytes = Base64.getUrlDecoder.decode(base)
            new String(bytes, "utf-8")
        }
        val url =
          if(request.path == "/"){
            s"https://scholar.google.com"
          } else {
            s"https://scholar.google.com${request.path}?${refinedRawQueryStr}"
          }
        val req = ws.url(url).withRequestTimeout(15 seconds).withMethod("GET").withHeaders(headers.toList: _*)
        req
          //.withRequestFilter(AhcCurlRequestLogger())
          .stream().flatMap {
          case StreamedResponse(response, body) =>
            //Refine response headers
            val respHeaders = response.headers.map{
              case (k, seq) if k.trim.toLowerCase == "set-cookie" =>
                (k, seq.map(v => v.replaceAll("""domain=[^;=]+\.com;?""", "")))
              case other => other
            }

            if (response.status >= 200 && response.status < 300) {
              // If there's a content length, send that, otherwise return the body chunked
              response.headers.find(t => t._1.trim.toLowerCase == "content-length").map(_._2) match {
                case Some(Seq(length)) =>
                  Future.successful(Ok.sendEntity(HttpEntity.Streamed(body, Some(length.toLong), None)).withHeaders(respHeaders.map(t => (t._1, t._2.mkString("; "))).toList: _*))
                case _ =>
                  Future.successful(Ok.chunked(body).withHeaders(respHeaders.map(t => (t._1, t._2.mkString("; "))).toList: _*))
              }
            } else if(response.status >= 300 && response.status < 500) {
              Future.successful{
                Status(response.status)
                  .withHeaders(respHeaders.filter(t => t._1.trim.toLowerCase == "location" || t._1.trim.toLowerCase == "set-cookie").map(t => (t._1, t._2.mkString("; "))).toList: _*)
              }
            } else {
              Future.successful(InternalServerError("Sorry, server return " + response.status))
            }
        }
    }
  }

  def post(path: String) = Action {
    Results.NoContent
  }

  def blackHole(path: String) = Action {
    Results.NoContent
  }

  def robots = Action {
    Ok("""User-agent: *
         |Disallow: /
       """.stripMargin
    )
  }
}
