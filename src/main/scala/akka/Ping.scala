package akka

import akka.actor.{Props, ActorSystem, ActorRef, Actor}
import akka.actor.Actor.Receive

/**
 * Created by paullawler on 2/18/15.
 */

case object PingMessage
case object PongMessage
case object StartMessage
case object StopMessage

class Ping(pong: ActorRef) extends Actor {

  var count = 0

  def increment = {
    count += 1
    println("Ping")
    println("Count is now " + count)
  }

  override def receive = {
    case StartMessage =>
      increment
      pong ! PingMessage
    case PongMessage =>
      increment
      if (count > 9) {
        pong ! StopMessage
        println("Ping stopped")
        context.stop(self)
      } else {
        pong ! PingMessage
      }
  }

}

class Pong extends Actor {
  override def receive: Receive = {
    case PingMessage =>
      println("Pong")
      sender ! PongMessage
    case StopMessage =>
      println("Pong stopped")
      context.stop(self)
  }
}

object PingPongTest extends App {
  val system = ActorSystem("PingPongSystem")
  val pong = system.actorOf(Props[Pong], name = "pong")
  val ping = system.actorOf(Props(new Ping(pong)), name = "ping")

  ping ! StartMessage
}