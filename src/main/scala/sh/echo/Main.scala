package sh.echo

import scala.concurrent.duration._

import com.typesafe.config.ConfigFactory
import net.mtgto.irc._

object Main extends App {
  import Actors.system.dispatcher

  val rootConfig = ConfigFactory.load()
  val clientConfig = rootConfig.getObject("scala-irc-bot").toConfig
  val client = new DefaultClient(clientConfig)

  // install shutdown hook for the actor system
  Actors.system.scheduler.schedule(1 second, 1 second) {
    if (!client.isConnected)
      Actors.system.shutdown()
  }

  client.connect()
}
