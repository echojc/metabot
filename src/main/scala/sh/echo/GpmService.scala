package sh.echo

import scala.concurrent.Future

import spray.client.pipelining._
import spray.http._
import spray.httpx.unmarshalling._
import spray.httpx.SprayJsonSupport._
import spray.json._

object GpmService {
  implicit val arf = Actors.system
  import arf.dispatcher

  val host = "bv.lan"
  val port = 8080

  object SearchResult extends DefaultJsonProtocol {
    implicit def jf = jsonFormat5(SearchResult.apply)
  }
  case class SearchResult(
    id: String,
    title: String,
    artist: String,
    album: String,
    durationMillis: Int
  ) {
    override def toString = s"$artist - $title ($album) [id: $id]"
  }

  def search(query: String): Future[List[SearchResult]] = {
    val pipeline: HttpRequest ⇒ Future[List[SearchResult]] = (
      sendReceive ~> unmarshal[List[SearchResult]]
    )
    pipeline(Get(Uri(s"http://$host:$port/search").withQuery("q" → query)))
  }

  def unmarshalOption[T: FromResponseUnmarshaller]: HttpResponse ⇒ Option[T] = {
    response ⇒
      if (response.status == StatusCodes.NotFound)
        None
      else
        Option(unmarshal[T](implicitly[FromResponseUnmarshaller[T]])(response))
  }

  def info(id: String): Future[Option[SearchResult]] = {
    val pipeline: HttpRequest ⇒ Future[Option[SearchResult]] = (
      sendReceive ~> unmarshalOption[SearchResult]
    )
    pipeline(Get(Uri(s"http://$host:$port/info/$id")))
  }
}

