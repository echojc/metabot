scalaVersion := "2.11.1"

version := "0.0.1-SNAPSHOT"

fork in run := true

resolvers ++= Seq(
  "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "spray repo" at "http://repo.spray.io"
)

libraryDependencies ++= Seq(
  "sh.echo" %% "scala-irc-bot" % "0.0.3",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.3",
  "io.spray" %% "spray-client" % "1.3.1-20140423",
  "io.spray" %% "spray-json" % "1.2.6"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding",
  "UTF8",
  "-feature",
  "-language:postfixOps"
)

initialCommands := """
  import sh.echo.echobot._
  import akka.actor._
  import scala.concurrent._
  import duration._
  import ExecutionContext.Implicits.global
"""
