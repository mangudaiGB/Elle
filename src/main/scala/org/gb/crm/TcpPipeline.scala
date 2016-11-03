
package org.gb.crm

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp.{Received, Write}
import akka.util.ByteString

/**
	* Created by neo on 22/10/16.
	*/

trait TcpPipeline {
	self ⇒
		def inbound(bytes: ByteString): List[ByteString]
		def outbound(bytes: ByteString): List[ByteString]

		def compose(other: TcpPipeline) = new TcpPipeline {
			override def inbound(bytes: ByteString): List[ByteString] = self.inbound(bytes).flatMap(x ⇒ other.inbound(x))

			override def outbound(bytes: ByteString): List[ByteString] = self.outbound(bytes).flatMap(x ⇒ other.outbound(x))
		}
}

object PassthroughTcpPipeline extends TcpPipeline {
	def inbound(bytes: ByteString) = bytes :: Nil
	def outbound(bytes: ByteString) = bytes :: Nil
}

class LoggingTcpPipeline(name: String) extends TcpPipeline {
	def inbound(bytes: ByteString) = {
		print(s">>> [$name] ${bytes.utf8String}")
		bytes :: Nil
	}

	def outbound(bytes: ByteString) = {
		print(s"<<< [$name] ${bytes.utf8String}")
		bytes :: Nil
	}
}

class DelimitedTcpPipeline(val delimiter: ByteString) extends TcpPipeline {
	var inboundBuffer = ByteString.empty
	var outboundBuffer = ByteString.empty

	override def inbound(bytes: ByteString): List[ByteString] = {
		val result = collect(Nil, inboundBuffer ++ bytes)
		inboundBuffer = result._2
		result._1
	}

	override def outbound(bytes: ByteString): List[ByteString] = {
		val result = collect(Nil, outboundBuffer ++ bytes)
		outboundBuffer = result._2
		result._1
	}

	private def collect(result: List[ByteString], bytes: ByteString): (List[ByteString], ByteString) = {
		extract(bytes) match {
			case (Some(head), tail) ⇒ collect(result ++ List(head), tail)
			case _ ⇒ (result, bytes)
		}
	}

	private def extract(b: ByteString): (Option[ByteString], ByteString) = b.indexOfSlice(delimiter) match {
		case i if i >= 0 ⇒ (Some(b.take(i + 2)), b.drop(i + 2))
		case _ ⇒ (None, b)
	}
}

class TcpPipelineAdapter(val left: ActorRef, val right: ActorRef, val pipeline: TcpPipeline) extends Actor {

	override def receive: Receive = {
		case r @ Received(data) if sender() == left ⇒ pipeline.inbound(data).foreach(right ! Received(_))
		case w @ Write(data, ack) if sender() == right ⇒ pipeline.outbound(data).foreach(left ! Write(_, ack))
		case x if sender() == left ⇒ right ! x
		case x if sender() == right ⇒ left ! x
	}
}
