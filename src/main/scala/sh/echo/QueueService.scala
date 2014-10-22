package sh.echo

import scala.concurrent.Future

import spray.client.pipelining._
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.json._
import JsonProtocol._

object QueueService {

  object RegisterUser {
    implicit def jf = jsonFormat1(RegisterUser.apply)
  }
  case class RegisterUser(
    nick: String)

  case class RegistrationFailed(nick: String) extends Exception(s"Could not register '$nick'.")

  object User {
    implicit def jf = jsonFormat2(User.apply)
  }
  case class User(nick: String, waitingSince: Option[Long])

  object Song {
    implicit def jf = jsonFormat1(Song.apply)
  }
  case class Song(data: String)

  case class QueueRequestRejected(userId: String, songId: String)
    extends Exception(s"Could not queue '$songId' for '$userId'.")

  object Queue {
    implicit def jf = jsonFormat1(Queue.apply)
    case class Named(userId: String, songId: String)
  }
  case class Queue(data: List[Map[String, String]]) {
    def named: List[Queue.Named] = (data map { tuple ⇒
      for {
        userId ← tuple.keys.headOption
        songId ← tuple.values.headOption
      } yield Queue.Named(userId, songId)
    }).flatten

    def tupled: List[(String, String)] = (data map { tuple ⇒
      for {
        userId ← tuple.keys.headOption
        songId ← tuple.values.headOption
      } yield (userId, songId)
    }).flatten
  }

  object NullableSong {
    implicit def jf = jsonFormat1(NullableSong.apply)
  }
  case class NullableSong(data: Option[String])

}
class QueueService(host: String, port: Int) {
  implicit val arf = Actors.system
  import arf.dispatcher
  import QueueService._

  def register(nick: String): Future[Unit] = {
    val pipeline: HttpRequest ⇒ Future[StatusCode] = (
      sendReceive
      ~> ((_: HttpResponse).status)
    )
    pipeline(Post(s"http://$host:$port/users", HttpEntity(`application/json`, RegisterUser(nick).toJson.prettyPrint))) map {
      case StatusCodes.Created ⇒ ()
      case _                   ⇒ throw RegistrationFailed(nick)
    }
  }

  def listUsers(): Future[Map[String, User]] = {
    import DefaultJsonProtocol._
    val pipeline: HttpRequest ⇒ Future[Map[String, User]] = (
      sendReceive
      ~> unmarshal[Map[String, User]]
    )
    pipeline(Get(s"http://$host:$port/users"))
  }

  def queue(userId: String, songId: String): Future[Unit] = {
    val pipeline: HttpRequest ⇒ Future[StatusCode] = (
      sendReceive
      ~> ((_: HttpResponse).status)
    )
    pipeline(Post(s"http://$host:$port/users/$userId/queue", HttpEntity(`application/json`, Song(songId).toJson.prettyPrint))) map {
      case StatusCodes.Created ⇒ ()
      case _                   ⇒ throw QueueRequestRejected(userId, songId)
    }
  }

  def showQueue(userId: String): Future[List[String]] = {
    import DefaultJsonProtocol._
    val pipeline: HttpRequest ⇒ Future[List[String]] = (
      sendReceive
      ~> unmarshal[List[String]]
    )
    pipeline(Get(s"http://$host:$port/users/$userId/queue"))
  }

  def showGlobalQueue(): Future[Queue] = {
    val pipeline: HttpRequest ⇒ Future[Queue] = (
      sendReceive
      ~> unmarshal[Queue]
    )
    pipeline(Get(s"http://$host:$port/queue"))
  }

  def lastPopped(): Future[Option[String]] = {
    val pipeline: HttpRequest ⇒ Future[NullableSong] = (
      sendReceive
      ~> unmarshal[NullableSong]
    )
    pipeline(Get(s"http://$host:$port/queue/last-pop")) map (_.data)
  }
}

