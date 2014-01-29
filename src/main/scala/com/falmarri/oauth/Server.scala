package com.falmarri.oauth

import java.io.File
import java.net.URLDecoder

import scala.collection.mutable.MapBuilder
import scala.util.{Try, Success, Failure}

import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpResponse
import org.jboss.netty.handler.codec.http.HttpResponseStatus
import org.jboss.netty.util.CharsetUtil

import com.twitter.finagle.Filter
import com.twitter.finagle.Http
import com.twitter.finagle.Service
import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.http.Method._
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Response
import com.twitter.finagle.http.path._
import com.twitter.finagle.http.service.RoutingService
import com.twitter.util.Await
import com.twitter.util.Future

import argonaut._
import argonaut.Argonaut._


//object Logger extends SimpleFilter[Request, Response] {
//
//  def apply(request: Request, service: Service[Request, Response]) = {
//    service(request)
//  }
//}


case class Config(key: File = new File("api.key"), 
    host: String = "gables.iplantcollaborative.org:80", 
    https: Boolean = false,
    client_id: String = "trellisapi",
    scope: Seq[String] = Seq("search"),
    port: String = "8081"
    )


object SearchServer{

  def main(args: Array[String]) {
    
    val parser = new scopt.OptionParser[Config]("groupy") {
       head("groupy", "x.x")
       opt[String]('h', "host") action { (x, c) =>
         c.copy(host=x) } text("The host that this server will connect to")
       opt[File]('k', "key") text("This application's private key file") action {(x,c) => 
         c.copy(key = x)}
       opt[Unit]("https") action { (_, c) => c.copy(https = true)}
       opt[String]('p', "port") action { (x, c) => c.copy(port=x)}
       arg[String]("<scope>...") optional() unbounded() action { (x, c) =>
         c.copy(scope = c.scope :+ x)}
     }
    
    val p = parser.parse(args, Config()).getOrElse { throw new Exception()}
    val client = Try {
      GroupyClient(ClientCredentials(p.key, p.client_id), p.host)
    }  match {
      case Success(v) =>
        v
      case Failure(e) =>
        GroupyClient(p.host)
    }

    val server = Http.serve(":" + p.port, nettyToFinagle andThen router(client))
    Await.ready(server)
  }
  
  private[this] val nettyToFinagle =
    Filter.mk[HttpRequest, HttpResponse, Request, Response] { (req, service) =>
      service(Request(req)) map { _.httpResponse }
    }

  def router(c:GroupyClient) =
    SearchRouter.byRequest { request =>
      (request.method, Path(request.path)) match {

        case Get -> Root / "api" / version / "users" / searchType / query =>
          JsonToResponse andThen
            BackwardsCompatFilter andThen
            JsonValidationFilter andThen
            SearchService(searchType, query, c)
        case _ => NotFoundService
      }

    }

  //val server = Http.serve(":8081", nettyToFinagle andThen Logger andThen router)
  //Await.ready(server)
  
  


}

object NotFoundService extends Service[Request, Response] {

  def apply(rawRequest: Request): Future[Response] = {
    Future.value(Response(HttpResponseStatus.NOT_FOUND))
  }
}

object SearchService {

  //lazy val client = GroupyClient()

  def apply(searchType: String, query: String, client: GroupyClient) = {
    searchType.toLowerCase() match {

      case search @ "name" => URLDecoder.decode(query, "UTF-8").split(" ").toList match {
        case first :: last :: Nil => Search(client, None, Seq(s"givenname:${first}*", s"sn:${last}*"))
        case first :: Nil => Search(client, None, Seq(s"givenname:${first}*"))
        case q @ _ => Search(client, Option(search))
      }
      case "email" => Search(client, Option(s"${query}*"), Seq("mail"))
      case "username" => Search(client, Option(s"${query}*"), Seq("username"))
      case _ => Search(client, Option(s"${query}*"))

    }
  }
}

case class Search(client: GroupyClient, query: Option[String], fields: Seq[String] = Seq())
  extends Service[Request, Option[Json]] {

  def apply(rawRequest: Request) = {
    client.get("users", ("search" -> query) +: fields.map { f => "field" -> f })
      .map {
        r =>
          Parse.parseOption(r.getContent().toString(CharsetUtil.UTF_8))
      }
  }
}

case object JsonToResponse extends Filter[Request, Response, Request, Json] {

  def apply(request: Request, service: Service[Request, Json]): Future[Response] = {

    service(request).map { json =>
      val r = Response(HttpResponseStatus.OK)
      r.setContentString(json.toString)
      r.setContentTypeJson
      r
    }
  }

}

case object JsonValidationFilter extends Filter[Request, Json, Request, Option[Json]] {

  def apply(request: Request, service: Service[Request, Option[Json]]): Future[Json] = {
    service(request).flatMap { json =>
      json match {
        case Some(j) => Future.value(j)
        case None => Future.exception(new Exception())
      }
    }
  }
}

case object BackwardsCompatFilter extends Filter[Request, Json, Request, Json] {

  val conversionMap = Map("sn" -> "lastname", "mail" -> "email", "o" -> "institution",
    "givenname" -> "firstname")

  def apply(request: Request, service: Service[Request, Json]): Future[Json] = {

    service(request).flatMap { json =>
      Future.value(
        Json("users" ->
          Json.array(
            json.arrayOrEmpty.map { j =>
              Json(
                j.objectOrEmpty.toMap.foldLeft(Map[JsonField, Json]("id" -> j.fieldOrNull("username"))) {
                  case (acc, (key, value)) =>
                    acc.updated(conversionMap.getOrElse(key, key), j.fieldOrNull(key))
                }.toSeq: _*)
            }.toSeq: _*)))
    }
  }
}

object SearchRouter {
  def byRequest[REQUEST](routes: PartialFunction[Request, Service[REQUEST, Response]]) =
    new RoutingService(
      new PartialFunction[Request, Service[REQUEST, Response]] {
        def apply(request: Request) = routes(request)
        def isDefinedAt(request: Request) = routes.isDefinedAt(request)
      })
}