package sh.echo

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Success

import spray.client.pipelining._
import spray.http._
import spray.httpx.unmarshalling._
import spray.httpx.SprayJsonSupport._
import spray.json._

object GpmService {
  import JsonProtocol._

  object SearchResult {
    implicit def jf = jsonFormat5(SearchResult.apply)
  }
  case class SearchResult(
      id: String,
      title: String,
      artist: String,
      album: String,
      durationMillis: Int) {
    override def toString = s"$artist - $title ($album)"
  }

  case class SongNotFound(songId: String) extends Exception(s"Could not resolve song id '$songId'.")
}
class GpmService(host: String, port: Int) {
  import JsonProtocol._

  implicit val arf = Actors.system
  import arf.dispatcher
  import GpmService._

  val _infoCache: mutable.Map[String, SearchResult] = mutable.Map.empty

  def search(query: String): Future[List[SearchResult]] = {
    val pipeline: HttpRequest ⇒ Future[List[SearchResult]] = (
      sendReceive ~> unmarshal[List[SearchResult]]
    )

    pipeline(Get(Uri(s"http://$host:$port/search").withQuery("q" → query))) andThen {
      case Success(searchResults) ⇒
        _infoCache.synchronized {
          searchResults foreach { searchResult ⇒
            _infoCache.update(searchResult.id, searchResult)
          }
        }
    }
  }

  //object AlbumSearchResult extends DefaultJsonProtocol {
  //  implicit def jf = jsonFormat3(AlbumSearchResult.apply)
  //}
  //case class AlbumSearchResult(
  //  id: String,
  //  title: String,
  //  artist: String
  //)

  //def searchAlbum(query: String): Future[List[AlbumSearchResult]] = {
  //  val pipeline: HttpRequest ⇒ Future[List[AlbumSearchResult]] = (
  //    sendReceive ~> unmarshal[List[AlbumSearchResult]]
  //  )

  //  pipeline(Get(Uri(s"http://$host:$port/search").withQuery("q" → query, "t" → "album")))
  //}

  def unmarshalOption[T: FromResponseUnmarshaller]: HttpResponse ⇒ Option[T] = {
    response ⇒
      if (response.status == StatusCodes.NotFound)
        None
      else
        Option(unmarshal[T](implicitly[FromResponseUnmarshaller[T]])(response))
  }
  def info(id: String): Future[SearchResult] = {
    val pipeline: HttpRequest ⇒ Future[Option[SearchResult]] = (
      sendReceive ~> unmarshalOption[SearchResult]
    )

    _infoCache.get(id) match {
      case Some(cachedResult) ⇒
        Future.successful(cachedResult)
      case None ⇒
        pipeline(Get(Uri(s"http://$host:$port/info/$id"))) map {
          case Some(searchResult) ⇒
            _infoCache.synchronized {
              _infoCache.update(searchResult.id, searchResult)
            }
            searchResult
          case None ⇒ throw SongNotFound(id)
        }
    }
  }
}

