

package org.gb.crm

import java.net.InetSocketAddress

import akka.actor.{ActorSystem, Props}

/**
	* Created by neo on 28/10/16.
	*/
object ApplicationServer extends App {

	implicit val system = ActorSystem("ELLE")
	val socket = new InetSocketAddress("0.0.0.0", 8888)
	system.actorOf(Props(new TcpServer(socket, conn ⇒ Props(new PredictiveServer(conn)))), "Prediction")

//	system.actorOf(Props(new TcpServer(socket, conn ⇒ Props(new OutputServer(conn)))), "Prediction")
}
