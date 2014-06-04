package sh.echo

import scala.concurrent.Future

import spray.client.pipelining._
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.json._

object QueueService {
  implicit val arf = Actors.system
  import arf.dispatcher

  val host = "fg.lan"
  val port = 3000

  object RegisterUser extends DefaultJsonProtocol {
    implicit def jf = jsonFormat1(RegisterUser.apply)
  }
  case class RegisterUser(
    nick: String
  )

  def register(nick: String): Future[Boolean] = {
    val pipeline: HttpRequest ⇒ Future[Boolean] = (
      sendReceive
      ~> ((_: HttpResponse).status == StatusCodes.Created)
    )
    pipeline(Post(s"http://$host:$port/users", HttpEntity(`application/json`, RegisterUser(nick).toJson.prettyPrint)))
  }

  object User extends DefaultJsonProtocol {
    implicit def jf = jsonFormat2(User.apply)
  }
  case class User(
    nick: String,
    waitingSince: Option[Long]
  )

  def listUsers(): Future[Map[String, User]] = {
    import DefaultJsonProtocol._
    val pipeline: HttpRequest ⇒ Future[Map[String, User]] = (
      sendReceive
      ~> unmarshal[Map[String, User]]
    )
    pipeline(Get(s"http://$host:$port/users"))
  }

  object Song extends DefaultJsonProtocol {
    implicit def jf = jsonFormat1(Song.apply)
  }
  case class Song(
    data: String
  )

  def queue(userId: String, songId: String): Future[Boolean] = {
    val pipeline: HttpRequest ⇒ Future[Boolean] = (
      sendReceive
      ~> ((_: HttpResponse).status == StatusCodes.Created)
    )
    pipeline(Post(s"http://$host:$port/users/$userId/queue", HttpEntity(`application/json`, Song(songId).toJson.prettyPrint)))
  }

  def listQueue(userId: String): Future[List[String]] = {
    import DefaultJsonProtocol._
    val pipeline: HttpRequest ⇒ Future[List[String]] = (
      sendReceive
      ~> unmarshal[List[String]]
    )
    pipeline(Get(s"http://$host:$port/users/$userId/queue"))
  }
}

