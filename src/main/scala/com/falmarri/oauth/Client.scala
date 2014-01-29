package com.falmarri.oauth

import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import com.twitter.finagle.{ Service, SimpleFilter, Http}
import com.twitter.finagle.builder.{ClientBuilder}
import com.twitter.conversions.time._
import com.twitter.util.Future
import scala.concurrent.stm._
import org.jboss.netty.handler.codec.http.DefaultHttpRequest
import org.jboss.netty.handler.codec.http.HttpVersion
import org.jboss.netty.handler.codec.http.HttpMethod
import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpResponse
import com.twitter.finagle.http.{Response, Request}
import com.twitter.util.Await
import com.twitter.finagle.client.DefaultClient
import com.twitter.finagle.client.Bridge
import com.twitter.finagle.dispatch.SerialClientDispatcher
import com.twitter.finagle.HttpTransporter
import com.twitter.finagle.http.codec.HttpClientDispatcher
import com.twitter.finagle.HttpRichClient
import com.twitter.finagle.Filter
import com.twitter.finagle.netty3._
import com.twitter.finagle.http.RichHttp
import com.twitter.finagle.http
import java.net.InetSocketAddress
import com.twitter.finagle.http.RequestBuilder
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import java.net.URLEncoder.encode
import java.nio.charset.Charset
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import com.twitter.finagle.HttpClient
import com.twitter.finagle.http.RequestConfig.Yes
import argonaut._
import argonaut.Argonaut._

object HandleErrors extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {

      service(request) flatMap { response =>
        response.getStatus.getCode match {
      
          case x if (x >= 200 && x < 300) =>
            Future.value(response)
          case x if (x == 403) => 
            Future.exception(OauthFailure())
          case code =>
            Future.exception(HttpStatusException(code, response.getStatus.getReasonPhrase, response))
        }
    }
  }
}


case class OauthFailure() extends Exception

case class HttpStatusException(code: Int, reason: String, response: HttpResponse)
  extends RuntimeException {
  var clientId = ""
  def addName(name: String) = {
    clientId = " in " + name
    this
  }

  def asString: String = response.toString

  override def getMessage(): String = {
    "HttpStatusException%s: Code: %d Reason: %s Body: %s" format(clientId, code, reason, asString)
  }
}

class OAuthFilter(c: ClientCredentials)
	extends SimpleFilter[HttpRequest, HttpResponse] {
	
	  private[this] var t: Ref[oauth.OAuth.Token] = Ref(None)
  
	  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
	    
	    val oauth = atomic { implicit txn =>
	      	t() orElse(retry)
    	} orAtomic { implicit txn =>
	      	t() = 
	      	    Parse.parseOr(
	      	        Await.result(GroupyClient.getToken(service, c)),
	      	        _.field("access_token").flatMap(_.string),
	      	        None)
	      	
	      	t()
	    }
    	oauth.foreach(o => request.headers.add("Authorization", "Bearer " + o))
	    
	    service(request) map { response =>
	      response.getStatus match {
	    	case FORBIDDEN => 
	    	  t.single.set(None)
	    	case _ => 
	      }
	      response
    	}
	  }
  
}

object ClientLogger extends SimpleFilter[HttpRequest, HttpResponse] {
  
  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    
    println(request)
    service(request)

  }
}

object GroupyClient {

  	def from( f: => Service[HttpRequest, HttpResponse]) = {}
  
  	def apply(
  	    credentials: ClientCredentials ,
  	    host: String) = {
  	  
  	  new GroupyClient(
  	      new OAuthFilter(credentials) andThen HttpClient.newService(host),
  	      credentials.iss,
  	      Option(credentials),
  	      "http://" + host
  	      )
  	}
  	
  	def apply(
  	    host: String) = {
  	  
  	  new GroupyClient(
  	      HttpClient.newService(host),
  	      "",
  	      None,
  	      "http://" + host
  	      )
  	}
  	
	implicit def encodePostData[V](data: Map[String, String]) =
	  data map {v =>
	    encode(v._1, "UTF-8") + "=" + encode(v._2, "UTF-8")
	    } mkString "&"


    def getToken(s: Service[HttpRequest, HttpResponse], c: ClientCredentials): Future[String] = {
	   
		s(
	      RequestBuilder()
	   		.url("http://localhost:8080/o/oauth2/token")
	   		.buildPost(
	   		    copiedBuffer(
	   		        encodePostData(Map(
	   		            ("assertion" -> c.sign(None, Seq("search"))),
	   		            ("grant_type" -> c.grant)
	   		            )).getBytes))
	      ) map { _.getContent().toString(Charset.forName("UTF-8")) }
	  
	}
}

trait GClient {
	val client: Service[HttpRequest, HttpResponse]
	val host: Uri
  
    def apply(request: HttpRequest) = client(request)
    
    def apply(implicit r: (RequestBuilder[_,_]) => HttpRequest): Future[HttpResponse] = this(r(RequestBuilder().url(host)))

    def get(loc: String)( r: (RequestBuilder[Yes,Nothing]) => HttpRequest) = this(r(RequestBuilder().url(host / loc)))

    def get( params: Map[String,Any])(implicit i1:DummyImplicit): Future[HttpResponse] = this(RequestBuilder().url(host.addParams(params.toList)).buildGet())

    def get( loc: String, params: Seq[(String,Any)])(implicit i1:DummyImplicit): Future[HttpResponse] = get((host / loc).addParams(params))
  
    def get( implicit loc: Uri) = this(RequestBuilder().url(loc).buildGet())

}




class GroupyClient(
    val client: Service[HttpRequest, HttpResponse],
    val name : String,
    val credentials: Option[ClientCredentials] = None,
    val host: Uri = "http://howe.iplantcollaborative.org:8080") extends GClient{
  
  import GroupyClient._

  def getToken() = {
		  		
    credentials match {
      
      case Some(c) => {
		  		this.get("/o/oauth2/token"){ r => 
  
    			r.buildPost(
		   		    copiedBuffer(
		   		        encodePostData(
		   		            Map(
			   		            ("assertion" -> c.sign(None, Seq("search"))),
			   		            ("grant_type" -> c.grant)
	   		            		)
		   		            )
	   		            .getBytes))

			}
      }
      case None => Future.exception(OauthFailure())
    }
  }
}