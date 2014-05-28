package sh.echo

import scala.concurrent.Future

import spray.client.pipelining._
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.json._

object GpmService {

  val host = "10.5.1.5"
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
    override def toString = s"$artist - $title ($album)"
  }

  def search(query: String): Future[List[SearchResult]] = {
    implicit val arf = Actors.system
    import arf.dispatcher

    val pipeline: HttpRequest ⇒ Future[List[SearchResult]] = (
      sendReceive ~> unmarshal[List[SearchResult]]
    )
    pipeline(Get(Uri(s"http://$host:$port/search").withQuery("q" → query)))
  }
}

