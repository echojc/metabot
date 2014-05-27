package sh.echo

import akka.actor._

object Actors {
  lazy val system: ActorSystem = {
    val system = ActorSystem()
    sys.addShutdownHook(system.shutdown())
    system
  }
}

