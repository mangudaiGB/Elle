

package org.gb.crm

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp.{Bind, Bound, CommandFailed, Connected}
import akka.io.{IO, Tcp}

/**
	* Created by neo on 22/10/16.
	*/
class TcpServer(bind: InetSocketAddress, handler: ActorRef ⇒ Props) extends Actor with ActorLogging {
	implicit val system = context.system
	IO(Tcp) ! Bind(self, bind)

	def receive = {
		case Bound(local) ⇒
			log.debug("Bound to {}", local)

		case CommandFailed(_: Bind) ⇒
			log.error("Unable to bind to {}", bind)
			context.stop(self)

		case Connected(remote, local) ⇒
			log.debug("New connection from {}", remote)
			val connection = sender()
			context.actorOf(handler(connection))
	}

}
