package sh.echo.bots

import java.util.Date
import scala.concurrent._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import net.mtgto.irc.Bot
import net.mtgto.irc.Client
import net.mtgto.irc.event._
import org.ocpsoft.prettytime.PrettyTime
import org.slf4j.LoggerFactory

import sh.echo.Actors
import sh.echo.GpmService
import sh.echo.QueueService

object CommandParser {
  val CommandPrefix = "!"

  def apply(text: String): Option[(String, Seq[String])] = {
    if (text startsWith (CommandPrefix)) {
      val args: Seq[String] = text drop (CommandPrefix.length) split ("\\s+")
      Some((args.head, args.tail))
    } else {
      None
    }
  }
}

case class UserInfo(
  id: String,
  waitingSince: Option[Long]
)

class MetaBot extends Bot {
  import Actors.system.dispatcher
  val logger = LoggerFactory.getLogger(getClass)
  val prettyTime = new PrettyTime()

  var userIdCache: Map[String, UserInfo] = Map.empty
  def refreshUserList(): Future[Map[String, UserInfo]] = {
    QueueService.listUsers() map { users ⇒
      val cache = users map { case (id, user) ⇒ (user.nick → UserInfo(id, user.waitingSince)) }
      userIdCache = cache
      cache
    }
  }
  def resolveNick(nick: String): Future[Option[UserInfo]] = {
    val idOption = userIdCache.get(nick)
    idOption match {
      case Some(id) ⇒ Future.successful(Option(id))
      case None ⇒ refreshUserList() map (_.get(nick))
    }
  }

  var recentSearchResults: Map[String, List[GpmService.SearchResult]] = Map.empty

  def search(nick: String, query: String)(reply: String ⇒ Unit): Unit = {
    logger.debug(s"searching for [$query]")
    GpmService.search(query) andThen {
      case Success(results) ⇒
        logger.debug(s"caching search results for [$query] by [$nick]: [$results]")
        recentSearchResults = recentSearchResults.updated(nick, results)
        if (!results.isEmpty) {
          reply(s"Search results for '$query':")
          results.zipWithIndex foreach { case (row, index) ⇒
            reply(s"${index+1}: $row")
          }
        } else {
          reply("No results.")
        }
      case Failure(e) ⇒
        logger.error(s"failed to search [$query]: [$e]")
    }
  }

  def register(nick: String)(reply: String ⇒ Unit): Unit = {
    QueueService.register(nick) flatMap (_ ⇒ resolveNick(nick)) andThen {
      case Success(Some(UserInfo(id, _))) ⇒
        logger.debug(s"registered [${nick}] as [${id}]")
        reply(s"Registered ${nick} (${id})")
      case Success(None) ⇒
        logger.debug(s"failed to register [${nick}]: nick not found in user list")
        reply("Failed to register :(")
      case Failure(e) ⇒
        logger.error(s"failed to register [${nick}]: [$e]")
    }
  }

  def listUsers(reply: String ⇒ Unit): Unit = {
    refreshUserList() andThen {
      case Success(users) ⇒
        logger.debug(s"registered users: [$users]")
        reply("Registered users:")
        users foreach { case (nick, UserInfo(id, waitingSince)) ⇒
          val since = waitingSince.fold("never")(t ⇒ prettyTime.format(new Date(t)))
          reply(s"* $nick ($id) (waiting since: $since)")
        }
      case Failure(e) ⇒
        logger.error(s"failed to retrieve user list: [$e]")
    }
  }

  def queue(nick: String, songId: String)(reply: String ⇒ Unit): Unit = {
    resolveNick(nick) andThen {
      case Success(Some(userInfo)) ⇒
        val requestSongId = (for {
          index               ← Try(songId.toInt).toOption
          recentSearchResults ← recentSearchResults.get(nick)
          searchResult        ← recentSearchResults.drop(index-1).headOption
        } yield searchResult.id).getOrElse(songId)

        QueueService.queue(userInfo.id, requestSongId) andThen {
          case Success(isSuccess) ⇒
            if (isSuccess)
              reply(s"queued song id: $songId for $nick")
            else
              reply(s"could not queue song id $songId")
          case Failure(e) ⇒
            logger.error(s"failed to retrieve user list: [$e]")
        }
      case Success(None) ⇒
        logger.debug(s"nick [$nick] wasn't in user list")
        reply("register first you silly sausage")
      case Failure(e) ⇒
        logger.error(s"failed to retrieve user list: [$e]")
    }
  }

  def sendMessage(client: Client, channel: String, text: String) =
    client.sendMessage(channel, s"[MetaBot]: $text")

  override def onMessage(client: Client, message: Message) = {
    val nick = message.nickname
    val chan = message.channel
    CommandParser(message.text) match {
      case Some((command, args)) ⇒
        command match {
          case "search" ⇒
            val query = args.mkString(" ").trim
            if (!query.isEmpty) {
              search(nick, query)(sendMessage(client, chan, _))
            } else {
              sendMessage(client, chan, "Please search for something.")
            }
          case "register" ⇒
            register(nick)(sendMessage(client, chan, _))
          case "users" ⇒
            listUsers(sendMessage(client, chan, _))
          case "queue" ⇒
            if (!args.isEmpty) {
              queue(nick, args(0))(sendMessage(client, chan, _))
            } else {
              sendMessage(client, chan, "Please specify a song id.")
            }
          case _ ⇒
            logger.debug(s"MetaBot got command [$command] with args [$args]")
        }
      case None ⇒
    }
  }

  override def onJoin(client: Client, join: Join) = {
    logger.info(s"joined channel: [${join.channel}]")
  }

  override def onPrivateMessage(client: Client, message: PrivateMessage) = { logger.trace(message.toString) }
  override def onNotice(client: Client, notice: Notice) = { logger.trace(notice.toString) }
  override def onInvite(client: Client, invite: Invite) = { logger.trace(invite.toString) }
  override def onKick(client: Client, kick: Kick) = { logger.trace(kick.toString) }
  override def onMode(client: Client, mode: Mode) = { logger.trace(mode.toString) }
  override def onTopic(client: Client, topic: Topic) = { logger.trace(topic.toString) }
  override def onNickChange(client: Client, nickChange: NickChange) = { logger.trace(nickChange.toString) }
  override def onOp(client: Client, op: Op) = { logger.trace(op.toString) }
  override def onPart(client: Client, part: Part) = { logger.trace(part.toString) }
  override def onQuit(client: Client, quit: Quit) = { logger.trace(quit.toString) }
  override def onTimer(client: Client) = { logger.trace("timer!") }

  override def onUnload(client: Client) = { logger.debug("unload!") }
  override def onDisconnect(client: Client) = { logger.debug("disconnect!") }
}
