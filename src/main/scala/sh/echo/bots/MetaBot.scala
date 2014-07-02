package sh.echo.bots

import java.util.Date
import scala.collection.mutable
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

object MetaBot {
  case class UserInfo(
    id: String,
    waitingSince: Option[Long]
  )

  case class MissingUserInfo(nick: String) extends Exception(s"Could not find user info for '$nick'.")
  case class MissingSongInfo(songId: String) extends Exception(s"Could not resolve song info for '$songId'.")

  val CommandPrefix = "!"
  def parseCommand(text: String): Option[(String, Seq[String])] = {
    if (text startsWith (CommandPrefix)) {
      val args: Seq[String] = text drop (CommandPrefix.length) split ("\\s+")
      Some((args.head, args.tail))
    } else {
      None
    }
  }
}

class MetaBot extends Bot {
  import MetaBot._
  import Actors.system.dispatcher
  val logger = LoggerFactory.getLogger(getClass)
  val prettyTime = new PrettyTime()

  var userIdCache: Map[String, UserInfo] = Map.empty
  def refreshUserCache(): Future[Map[String, UserInfo]] =
    QueueService.listUsers() map { users ⇒
      val cache = users map { case (id, user) ⇒ (user.nick → UserInfo(id, user.waitingSince)) }
      userIdCache = cache
      cache
    }
  def resolveNick(nick: String): Future[UserInfo] =
    userIdCache.get(nick) match {
      case Some(id) ⇒
        Future.successful(id)
      case None ⇒
        refreshUserCache() map {
          _.get(nick) match {
            case Some(id) ⇒ id
            case None     ⇒ throw MissingUserInfo(nick)
          }
        }
    }

  var recentSearchResults: Map[String, List[GpmService.SearchResult]] = Map.empty

  def resolveSongNameWithFallback(songId: String): Future[String] =
    GpmService.info(songId) map (_.toString) recover { case _ ⇒ songId }

  def resolveSongNamesWithFallback(songIds: List[String]): Future[List[String]] =
    Future.sequence(songIds map (resolveSongNameWithFallback))

  def resolveSongName(songId: String): Future[String] =
    GpmService.info(songId) map (_.toString) recover { case _ ⇒ throw MissingSongInfo(songId) }

  def search(nick: String, query: String)(reply: String ⇒ Unit): Unit = {
    logger.debug(s"searching for [$query]")
    GpmService.search(query) andThen {
      case Success(results) ⇒
        logger.debug(s"caching search results for [$query] by [$nick]: [$results]")
        recentSearchResults = recentSearchResults.updated(nick, results)
        results match {
          case Nil         ⇒ reply("No results.")
          case results @ _ ⇒ results.zipWithIndex foreach { case (result, index) ⇒ reply(s"${index + 1}: $result") }
        }
      case Failure(e) ⇒
        logger.error(s"failed to search [$query]: [$e]")
        reply(s"""Searching for "$query" failed (unknown reason).""")
    }
  }

  def register(nick: String)(reply: String ⇒ Unit): Unit = {
    QueueService.register(nick) flatMap (_ ⇒ resolveNick(nick)) andThen {
      case Success(UserInfo(id, _)) ⇒
        logger.debug(s"registered [$nick] as [$id]")
        reply(s"Registered $nick.")
      case Failure(MissingUserInfo(nick)) ⇒
        logger.debug(s"failed to register [$nick]: nick not found in user list")
        reply(s"Registered $nick (but could not look up user info).")
      case Failure(QueueService.RegistrationFailed(nick)) ⇒
        logger.error(s"failed to register [$nick]")
        reply("Registration failed (queue did not accept registration).")
      case Failure(e) ⇒
        logger.error(s"unknown exception while registering [$nick]: [$e]")
        reply("Registration failed (unknown reason).")
    }
  }

  def listUsers(reply: String ⇒ Unit): Unit = {
    refreshUserCache() andThen {
      case Success(users) ⇒
        logger.debug(s"registered users: [$users]")
        reply("Registered users:")
        users foreach { case (nick, UserInfo(id, waitingSince)) ⇒
          val since = waitingSince.fold("never")(t ⇒ prettyTime.format(new Date(t)))
          reply(s"$id: $nick (waiting since: $since)")
        }
      case Failure(e) ⇒
        logger.error(s"failed to retrieve user list: [$e]")
        reply("Could not retrieve user list.")
    }
  }

  def parseRequestSongId(nick: String, songId: String): String =
    (for {
      index               ← Try(songId.toInt).toOption
      recentSearchResults ← recentSearchResults.get(nick)
      searchResult        ← recentSearchResults.drop(index-1).headOption
    } yield searchResult.id).getOrElse(songId)

  def queue(nick: String, songId: String)(reply: String ⇒ Unit): Unit =
    (for {
      userInfo      ← resolveNick(nick)
      requestSongId = parseRequestSongId(nick, songId)
      songInfo      ← resolveSongName(requestSongId)
      _             ← QueueService.queue(userInfo.id, requestSongId)
    } yield songInfo) andThen {
      case Success(songInfo) ⇒
        logger.debug(s"queued song [$songInfo]")
        reply(s"Queued song $songInfo for $nick.")
      case Failure(QueueService.QueueRequestRejected(userId, songId)) ⇒
        logger.debug(s"could not queue song [$songId] for user [$userId]")
        reply(s"Could not queue song for $nick.")
      case Failure(MissingSongInfo(songId)) ⇒
        logger.debug(s"tried to queue bad song id [$songId]")
        reply(s"Could not queue invalid song or queue id $songId.")
      case Failure(MissingUserInfo(nick)) ⇒
        logger.debug(s"couldn't resolve [$nick] to id for queuing")
        reply("Please register first (!register).")
      case Failure(e) ⇒
        logger.error(s"failed to queue song [$songId] for [$nick]: [$e]")
        reply("Failed to queue song (unknown error).")
    }

  def showQueue(nick: String)(reply: String ⇒ Unit): Unit =
    (for {
      userInfo  ← resolveNick(nick)
      songIds   ← QueueService.showQueue(userInfo.id)
      songInfos ← resolveSongNamesWithFallback(songIds)
    } yield songInfos) andThen {
      case Success(songInfos) ⇒
        logger.debug(s"retrieved queue for [$nick]: [$songInfos]")
        songInfos match {
          case Nil ⇒
            reply("No songs queued.")
          case songInfos @ _ ⇒
            reply(s"${nick}'s queue:")
            songInfos.zipWithIndex foreach { case (songInfo, index) ⇒ reply(s"${index + 1}: $songInfo") }
        }
      case Failure(MissingUserInfo(nick)) ⇒
        logger.debug(s"couldn't resolve [$nick] to id for retrieving user queue")
        reply(s"No user $nick.")
      case Failure(e) ⇒
        logger.error(s"failed to retrieve user queue of [$nick]: [$e]")
        reply(s"Failed to retrieve queue for $nick (unknown error).")
    }

  def showGlobalQueue(reply: String ⇒ Unit): Unit =
    (for {
      queue ← QueueService.showGlobalQueue()
      songs ← resolveSongNamesWithFallback(queue.data)
    } yield songs) andThen {
      case Success(songs) ⇒
        logger.debug(s"queried for global queue")
        songs match {
          case Nil ⇒
            reply("No songs queued.")
          case songs @ _ ⇒
            reply("Global queue:")
            songs.zipWithIndex foreach { case (song, index) ⇒ reply(s"${index + 1}: $song") }
        }
      case Failure(e) ⇒
        logger.error(s"failed to retrieve global queue: [$e]")
    }

  def info(songId: String)(reply: String ⇒ Unit): Unit = {
    GpmService.info(songId) andThen {
      case Success(songInfo) ⇒
        logger.debug(s"resolved song id [$songId] to [$songInfo]")
        reply(s"$songId: $songInfo")
      case Failure(MissingSongInfo(songId)) ⇒
        logger.debug(s"could not resolve song id [$songId]")
        reply(s"""Could not find info on id "$songId".""")
      case Failure(e) ⇒
        logger.error(s"failed to resolve song id [$songId]: [$e]")
        reply(s"Looking up song id failed (unknown reason).")
    }
  }

  def nowPlaying(reply: String ⇒ Unit): Unit = {
    QueueService.lastPopped() andThen {
      case Success(Some(songId)) ⇒
        GpmService.info(songId) andThen {
          case Success(songInfo) ⇒
            logger.debug(s"resolved last popped id [$songId] to [$songInfo]")
            reply(s"Now playing: $songInfo")
          case Failure(MissingSongInfo(songId)) ⇒
            logger.debug(s"could not resolve last popped id [$songId]")
            reply(s"Now playing: $songId (could not resolve song id)")
          case Failure(e) ⇒
            logger.error(s"failed to resolve last popped id [$songId]: [$e]")
            reply(s"Now playing: $songId (unknown failure).")
        }
      case Success(None) ⇒
        logger.debug(s"no last popped")
        reply(s"Nothing is playing.")
      case Failure(e) ⇒
        logger.error(s"failed to retrieve last popped: [$e]")
        reply(s"Looking up now playing failed (unknown reason).")
    }
  }

  val allowedChannels: mutable.Set[String] = mutable.Set("#music-queue")

  def sendMessage(client: Client, channel: String, text: String) =
    client.sendMessage(channel, s"[MetaBot]: $text")

  override def onMessage(client: Client, message: Message): Unit = {
    val nick = message.nickname
    val chan = message.channel

    if (!(allowedChannels contains (chan)))
      return

    val replyFun: String ⇒ Unit = sendMessage(client, chan, _)
    parseCommand(message.text) match {
      case Some((command, args)) ⇒
        command match {
          case "search" ⇒
            val query = args.mkString(" ").trim
            if (!query.isEmpty) {
              search(nick, query)(replyFun)
            } else {
              replyFun("Please search for something.")
            }
          case "register" ⇒
            register(nick)(replyFun)
          case "users" ⇒
            listUsers(replyFun)
          case "queue" ⇒
            if (!args.isEmpty) {
              queue(nick, args(0))(replyFun)
            } else {
              showGlobalQueue(replyFun)
            }
          case "show" ⇒
            if (!args.isEmpty) {
              showQueue(args(0))(replyFun)
            } else {
              showQueue(nick)(replyFun)
            }
          case "info" ⇒
            if (!args.isEmpty) {
              info(args(0))(replyFun)
            } else {
              replyFun("Please enter a song id.")
            }
          case "np" ⇒
            nowPlaying(replyFun)
          case "meta:channels" ⇒
            args foreach { channel ⇒
              if (channel startsWith ("remove:"))
                allowedChannels -= channel drop ("remove:".length)
              else
                allowedChannels += channel
            }
            sendMessage(client, chan, allowedChannels mkString (", "))
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
