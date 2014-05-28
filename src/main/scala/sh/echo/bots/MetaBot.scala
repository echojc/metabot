package sh.echo.bots

import scala.util.Failure
import scala.util.Success

import net.mtgto.irc.Bot
import net.mtgto.irc.Client
import net.mtgto.irc.event._
import org.slf4j.LoggerFactory

import sh.echo.Actors
import sh.echo.GpmService

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

class MetaBot extends Bot {
  val logger = LoggerFactory.getLogger(getClass)

  def sendMessage(client: Client, channel: String, text: String) =
    client.sendMessage(channel, s"[MetaBot]: $text")

  override def onMessage(client: Client, message: Message) = {
    import Actors.system.dispatcher

    CommandParser(message.text) match {
      case Some((command, args)) ⇒
        command match {
          case "search" ⇒
            val query = args.mkString(" ").trim
            if (!query.isEmpty) {
              logger.debug(s"searching for [$query]")
              GpmService.search(query) andThen {
                case Success(results) ⇒
                  logger.debug(s"got search results for [$query]: [$results]")
                  if (!results.isEmpty) {
                    sendMessage(client, message.channel, s"Search results for '$query':")
                    results foreach { row ⇒
                      sendMessage(client, message.channel, row.toString)
                    }
                  } else {
                    sendMessage(client, message.channel, "No results.")
                  }
                case Failure(e) ⇒
                  logger.error(s"failed to search [$query]: [$e]")
              }
            } else {
              sendMessage(client, message.channel, "Please search for something.")
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
