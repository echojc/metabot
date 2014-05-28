package sh.echo

import scala.concurrent.duration._

import com.typesafe.config.ConfigFactory
import net.mtgto.irc._
import org.slf4j.LoggerFactory

object Main extends App {
  import Actors.system.dispatcher
  val logger = LoggerFactory.getLogger(getClass)

  val rootConfig = ConfigFactory.load()
  val clientConfig = rootConfig.getObject("scala-irc-bot").toConfig
  val client = new DefaultClient(clientConfig)

  // install shutdown hook for the actor system
  var clientConnected = false
  Actors.system.scheduler.schedule(1 second, 1 second) {
    if (clientConnected && !client.isConnected) {
      logger.info("shutting down actor system...")
      Actors.system.shutdown()
      System.exit(0)
    }
  }

  client.connect()
  clientConnected = true
}
