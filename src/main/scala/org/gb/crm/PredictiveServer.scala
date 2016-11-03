

package org.gb.crm

import akka.actor.{ActorRef, FSM, Props}
import akka.io.Tcp._
import akka.util.ByteString
import org.gb.crm.data.{BeautifyPrint, Table, UserRow, UserTable}

import scala.util.Random

/**
	* Created by neo on 28/10/16.
	*/

trait Context {
	def getWeight: Float
	def criticalWeight: Float
}
case class User(userId: Int, firstName: String, lastName: String, level: Int, companyId: Int) {
	def fullname: String = firstName + " " + lastName
}
case class Company(companyId: Int, name: String, strength: Int, domain: String, budget: Int)
case class SearchContext() extends Context {

	var keywordList:List[(String, Int)] = List()

	def addNewKeyword(keyword: String, weight: Int) = keywordList = keywordList :+ (keyword, weight)

	override def getWeight: Float = keywordList.foldLeft(0)(_ + _._2)/keywordList.size

	override def criticalWeight: Float = 3.4f
}
case class SocialContext() {
	var neighbourMap: List[(User, Float, Float)] = List()

	def addNewEntry(user: User, weight: Float) = neighbourMap = neighbourMap :+ (user, weight, 0f)
	def data: List[(User, Float, Float)] = neighbourMap
	def update(con: String, weight: Float) = {
		con match {
			case "mail" ⇒ neighbourMap = neighbourMap.map {
				case (u, l, w) ⇒ (u, l, w + l*weight*0.2f)
			}
			case "find" ⇒ neighbourMap = neighbourMap.map {
				case (u, l, w) ⇒ (u, l, w + l*weight*0.3f)
			}
			case "site" ⇒ neighbourMap = neighbourMap.map {
				case (u, l, w) ⇒ (u, l, w + l*weight*0.5f)
			}
		}
	}
}
case class EmailDigitalContext() extends Context {
	var emailList: List[(Int, String, Int)] = List()

	def addNewEmail(id: Int, email: String, weight: Int) = {
		emailList = emailList :+ (id, email, weight)
	}

	override def getWeight: Float = emailList.foldLeft(0)(_ + _._3)/emailList.size

	override def criticalWeight: Float = 2.1f
}
case class WebsiteDigitalContext() extends Context {
	var webList: List[(String, Int, Int)] = List()

	def addNewWebPage(page: String, weight: Int, timeSpent: Int) = webList = webList :+ (page, weight, timeSpent)

	def getWeight: Float = webList.foldLeft(0f)((s, x) ⇒ s + x._2.toFloat * x._3.toFloat)/(webList.size*20)

	override def criticalWeight: Float = 24f
}

object PredictiveServer {
	val X = "X"
	private val userTable = Table.userTable
	private val companyTable = Table.companyTable
	private val searchTable = Table.searchTable
	private val emailTable = Table.emailTable
	private val websiteTable = Table.websiteTable

	sealed trait State
	case object ZeroState extends State
	case object InitializeState extends State
	case object IdentityState extends State
	case object CompanyState extends State
	case object SearchState extends State
	case object SocialState extends State
	case object EmailState extends State
	case object WebsiteState extends State

	sealed trait Data
	case object Empty extends Data
	case class Session(var user: Option[User] = None, var company: Option[Company] = None, var search: Option[SearchContext] = None,
										 var social: Option[SocialContext] = None, var email: Option[EmailDigitalContext] = None,
										 var web: Option[WebsiteDigitalContext] = None) extends Data
}

class PredictiveServer(connection: ActorRef) extends FSM[PredictiveServer.State, PredictiveServer.Data] {
	import org.gb.crm.PredictiveServer._

	private val pipeline = new DelimitedTcpPipeline(ByteString("\r\n")).compose(new LoggingTcpPipeline("PREDICTIVE SERVER"))
	private val adapter = context.actorOf(Props(new TcpPipelineAdapter(connection, self, pipeline)))

	private implicit var session:Session = Session()

	connection ! Register(adapter)
	self ! Register(self)

	when (ZeroState) (PartialFunction.empty)

	when (InitializeState) {
		case Event(Register(_, _, _), _) ⇒
			reply(200, "Initializing Engine.\n")
			session = Session()
			goto(IdentityState)
	}

	when (IdentityState) {
		case Event(Received(Command("user", d)), _) ⇒ {
			d match {
				case "-l" ⇒ {
					var response:Seq[UserRow] = Nil
					if (session.company.isEmpty) {
						response = for (i ← 1 to 30) yield userTable(Random.nextInt(1000))
					} else {
						response = userTable.filter(_._5 == session.company.get.companyId)
					}
					var message = ""
					for(s ← response) {
						message += BeautifyPrint.PrintUser(s.productIterator.toList) + "\n"
					}
					reply(200, message)
					stay()
				}
				case args:String if !args.isEmpty && args.contains(" ") ⇒ {
					args.split(" ") match {
						case Array(option, argument) ⇒ {
							option match {
								case "-c" ⇒ {
									val users = userTable.filter(_._5 == argument.toInt)
									val response = for(i ← 1 to 10) yield users(Random.nextInt(30))
									var message = ""
									for(s ← response) {
										message += BeautifyPrint.PrintUser(s.productIterator.toList) + "\n"
									}
									reply(200, message)
									stay()
								}
								case "-f" ⇒ ???
								case "-s" ⇒ {
									val user = userTable.filter(_._1 == argument.toInt).head
									session.user = Some(User(user._1, user._2, user._3, user._4, user._5))
									val company = companyTable.filter(_._1 == session.user.get.companyId).head
									session.company = Some(Company(company._1, company._2, company._3, company._4, company._5))
									reply(200, OutputPage.apply format (user._2 + " " + user._3, session.company.get.name,
											session.company.get.domain, session.company.get.budget, X, X, X, X, X, X, X, X, X))
									reply(200, "Execute 'cont' to compute social graph.")
									goto(CompanyState)
								}
								case "_" ⇒ {
									reply(400, "user command is wrong.\n" + ManPages("user"))
									stay()
								}
							}
						}
					}
				}
				case _ ⇒ {
					reply(400, "user command is wrong.\n" + ManPages("user"))
					stay()
				}
			}
		}
		case Event(Received(Command("comp", d)), _) ⇒ {
			d match {
				case "-l" ⇒ {
					val response = for (i ← 1 to 5) yield companyTable(Random.nextInt(20))
					var message = ""
					for(s ← response) {
						message += BeautifyPrint.PrintCompany(s.productIterator.toList) + "\n"
					}
					reply(200, message)
					stay()
				}
				case args: String if !args.isEmpty && args.contains(" ") ⇒ {
					args.split(" ") match {
						case Array(option, argument) ⇒ {
							option match {
								case "-s" ⇒ {
									val company = companyTable.filter(_._1 == argument.toInt).head
									session.company = Some(Company(company._1, company._2, company._3, company._4, company._5))
									reply(300, OutputPage.apply format (X, company._2, company._4, company._5, X, X, X, X, X, X, X, X, X))
									stay()
								}
							}
						}
					}
				}
				case _ ⇒ {
					reply(400, "comp command is wrong.\n" + ManPages("comp"))
					stay()
				}
			}
		}
	}

	when (CompanyState) {
		case Event(Received(Command("cont", d)), _) ⇒ {
			reply(800, "Setting the company details and converting the bipartite network to unipartite.")
			val user: User = session.user.get
			val companyUser = userTable.filter(_._5 == session.company.get.companyId)
				.filter{case (id, fn, ln, l, c) ⇒ (l - user.level < 4) && (l - user.level > 0)}
				.sortBy(_._4-user.level)
				.slice(0, 5)
			val weights = companyUser.map{case (id, fn, ln, l, c) ⇒ 1.0f/(l - user.level)}
			val socialContext: SocialContext = SocialContext()
			for(i ← 0 to companyUser.length - 1) {
				val user:User = User(companyUser(i)._1, companyUser(i)._2, companyUser(i)._3, companyUser(i)._4, companyUser(i)._5)
				socialContext.addNewEntry(user, weights(i))
			}
			session.social = Some(socialContext)
			val company = session.company.get
			var T_x:Array[String] = Array()
			for (so ← socialContext.data) {
				T_x = T_x :+ so._1.fullname + "[" + so._1.level + "]" + "[" + so._2 + "]"
			}
			for (x ← 0 to 5-socialContext.data.length) {
				T_x = T_x :+ "X"
			}
			reply(200, OutputPage.apply format (user.fullname, company.name, company.domain, company.budget,
				X, X, X, X, T_x(0), T_x(1), T_x(2), T_x(3), T_x(4)))
			reply(200, "Moving to search-mining stage. Use 'help find' to see options.")
			goto(SearchState)
		}
	}

	when (SearchState) {
		case Event(Received(Command("find", data)), _) ⇒
			data match {
				case "-l" ⇒ {
					val response = searchTable.filter(_._2 == session.company.get.domain).head
					val message = response._3 + "\n" + response._5 + "\n" + response._7
					reply(200, message)
					stay()
				}
				case "-c" ⇒ {
					reply(200, "Moving to mails.")
					goto(EmailState)
				}
				case args: String if !args.isEmpty && args.contains(" ") ⇒ {
					args.split(" ", 2) match {
						case Array(option, argument) ⇒ {
							option match {
								case "-s" ⇒ {
									val result:(String, Int) = searchTable.collect {
										case (id, d, s1, w1, s2, w2, s3, w3) if (d == session.company.get.domain && s1 == argument) ⇒ (s1, w1)
										case (id, d, s1, w1, s2, w2, s3, w3) if (d == session.company.get.domain && s2 == argument) ⇒ (s2, w2)
										case (id, d, s1, w1, s2, w2, s3, w3) if (d == session.company.get.domain && s3 == argument) ⇒ (s3, w3)
									}.head
									if (session.search.isEmpty) {
										val searchContext: SearchContext = SearchContext()
										searchContext.addNewKeyword(result._1, result._2)
										session.search = Some(searchContext)
										session.social.get.update("find", result._2)
									} else {
										if (session.search.get.keywordList.filter{case (x, y) ⇒ x == argument}.isEmpty) {
											session.search.get.addNewKeyword(result._1, result._2)
											session.social.get.update("find", result._2)
										}
									}
									reply(200, "Updated social graph.")
									stay()
								}
								case _ ⇒ {
									reply(400, "Unidentified command in find. Run 'help find' to know your options.")
									stay()
								}
							}
						}
					}
				}
				case _ ⇒ {
					reply(400, "Unidentified command in find. \n" + ManPages("find"))
					stay()
				}
			}
	}

	when (EmailState) {
		case Event(Received(Command("mail", args)), _) ⇒
			args match {
				case "-l" ⇒ {
					var message = ""
					val response: List[(Int, String, String, String)] = emailTable.map {
						case (id, sub, w, h, a) if (!h && !a) ⇒ (id, sub, "Hyperlink[N]", "Attachment[N]")
						case (id, sub, w, h, a) if (h && !a) ⇒ (id, sub, "Hyperlink[Y]", "Attachment[N]")
						case (id, sub, w, h, a) if (!h && a) ⇒ (id, sub, "Hyperlink[N]", "Attachment[Y]")
						case (id, sub, w, h, a) if (h && a) ⇒ (id, sub, "Hyperlink[Y]", "Attachment[Y]")
						case (id, sub, w, h, a) ⇒ (id, sub, "Y", "Y")
					}.toList
					for(s ← response) {
						message += BeautifyPrint.PrintEmail(s.productIterator.toList) + "\n"
					}
					reply(200, message)
					stay()
				}
				case "-c" ⇒ {
					reply(200, "Moving to the next stage: Website")
					goto(WebsiteState)
				}
				case args:String if !args.isEmpty && args.contains(" ") ⇒ {
					args.split(" ", 2) match {
						case Array(option, value) ⇒ {
							option match {
								case "-s" ⇒ {
									val email = emailTable.find(_._1 == value.toInt).head
									if (session.email.isEmpty) session.email = Some(EmailDigitalContext())
									val emailContext: EmailDigitalContext = session.email.get
									if (emailContext.emailList.filter(_._1 == email._1).headOption.isEmpty) {
										emailContext.addNewEmail(email._1, email._2 + "[H=" + email._4 + "][A=" + email._5 + "]", email._3)
										session.social.get.update("mail", emailContext.getWeight)
									}
									reply(200, "Mail effect computed.")
									stay()
								}
								case _ ⇒ {
									reply(200, "Wrong command. Run 'help mail' for more options.")
									stay()
								}
							}
						}
					}
				}
				case _ ⇒ {
					reply(200, "Wrong command. Run 'help mail' for more options.")
					stay()
				}
			}
	}

	when (WebsiteState) {
		case Event(Received(Command("site", args)), _) ⇒
			args match {
				case "-l" ⇒ {
					var message = ""
					val response: List[(Int, String, Int, String)] = websiteTable.map {
						case (id, page, w, h, a) if (h) ⇒ (id, page, w, "Hyperlinks: " + a.mkString(","))
						case (id, page, w, h, a) ⇒ (id, page, w, "Hyperlinks: 0")
					}.toList
					for(s ← response) {
						message += BeautifyPrint.PrintWebsite(s.productIterator.toList) + "\n"
					}
					reply(200, message)
					stay()
				}
				case args:String if !args.isEmpty && args.contains(" ") ⇒ {
					args.split(" ", 2) match {
						case Array(option, value) ⇒ {
							option match {
								case "-s" ⇒ {
									val web = websiteTable.find(_._1 == value.toInt).head
									if (session.web.isEmpty) session.web = Some(WebsiteDigitalContext())
									val webContext: WebsiteDigitalContext = session.web.get
									if (webContext.webList.filter(_._1 == web._1).headOption.isEmpty) {
										webContext.addNewWebPage(web._2, web._3, 10)
										session.social.get.update("site", webContext.getWeight)
									}
									reply(200, "Website visit computed.")
									stay()
								}
								case _ ⇒ {
									reply(200, "Wrong command. Run 'help site' for more options.")
									stay()
								}
							}
						}
					}
				}
				case _ ⇒ {
					reply(200, "Wrong command. Run 'help site' for more options.")
					stay()
				}
			}
	}

	whenUnhandled {
		case Event(Received(Command("pill", args)), _) ⇒ {
			args match {
				case "blue" ⇒ {
					reply(200, "Bye Cypher!!! ┻━┻ ︵ヽ(`Д´)ﾉ︵\uFEFF ┻━┻")
					stop()
				}
				case "red" ⇒ {
					reply(200, "Rebooting the Matrix...")
					session = Session()
					goto(IdentityState)
				}
				case _ ⇒ {
					reply(400, "You take the blue pill, the story ends. " +
					"You wake up in your bed and believe whatever you want to believe. " +
					"You take the red pill, you stay in Wonderland, and I show you how deep the rabbit hole goes.")
					stay()
				}
			}
		}
		case Event(Received(Command("echo", _)), s) ⇒ {
			var user: String = "X"
			var company = "X"
			var domain = "X"
			var budget = "X"
			if (!session.user.isEmpty) {
				user = session.user.get.fullname + "[" + session.user.get.level + "]"
			}
			if (!session.company.isEmpty) {
				company = session.company.get.name
				domain = session.company.get.domain
				budget = session.company.get.budget.toString
			}
			var T_x: Array[String] = Array.fill(5)("X")
			if (!session.social.isEmpty) {
				val social: SocialContext = session.social.get
				T_x = Array()
				for (so ← social.data) {
					T_x = T_x :+ so._1.fullname + "[" + so._1.level + "]" + "[" + so._2 + "]" + "[" + so._3 + "]"
				}
				for (x ← 0 to 5 - social.data.length) {
					T_x = T_x :+ "X"
				}
			}
			var searchWeight = 0f
			var emailWeight = 0f
			var webWeight = 0f
			if (!session.search.isEmpty) searchWeight = session.search.get.keywordList.map(_._2).sum
			if (!session.email.isEmpty) emailWeight = session.email.get.emailList.map(_._3).sum
			if (!session.web.isEmpty) webWeight = session.web.get.webList.map(_._2).sum
			val totalWeight = 0.2f * searchWeight + 0.3f * emailWeight + 0.5 * webWeight
			reply(200, OutputPage.apply format(user, company, domain, budget,
			searchWeight, emailWeight, webWeight, totalWeight, T_x(0), T_x(1), T_x(2), T_x(3), T_x(4)))
			stay()
		}
		case Event(Received(Command("help", d)), s) ⇒ {
			d match {
				case "user" | "comp" | "find" | "mail" | "site" | "pill" ⇒ {
					reply(200, ManPages(d))
					stay()
				}
				case _ ⇒ {
					reply(200, ManPages(d))
					log.info(s.toString)
					stay()
				}
			}
		}
		case Event(Received(Command("jump", args)), s) ⇒ {
			args match {
				case "find" ⇒ {
					reply(200, "Going to search stage.")
					goto(SearchState)
				}
				case "mail" ⇒ {
					reply(200, "Going to email stage.")
					goto(EmailState)
				}
				case "site" ⇒ {
					reply(200, "Going to website stage.")
					goto(WebsiteState)
				}
				case _ ⇒ {
					reply(400, "Refer to the help pages.")
					stay()
				}
			}
		}
		case Event(_: ConnectionClosed, _) ⇒ {
				log.debug("Connection closed")
				context.stop(self)
				goto(InitializeState)
			}
		case Event(_, s) ⇒ {
			reply(400, "Received unhandled request. Use 'help user/comp/site/find/mail")
			stay()
		}
	}

	startWith(InitializeState, Empty)
//	startWith(EmailState, Empty)
	initialize()

	def reply(code: Int, message: String = "Empty Message.") = {
		adapter ! Write(Reply(code, message))
	}
	def replyOk(message: String = "OK") = reply(250, message)
	def replyError(message: String = "Error") = reply(500, message)
}

sealed trait Reply {
	val code: Int
	val message: String
}
object Reply {
	val CRLF = ByteString("\r\n")

	def apply(code: Int, message: String): ByteString = {
		if (message.length > 0) ByteString(code.toString + "\n" + message) ++ CRLF
		else ByteString(code.toString) ++ CRLF
	}

	def unapply(raw: ByteString): Option[(Int, String)] = extractCode(raw) match {
		case Some(code) ⇒
			if (raw.endsWith(CRLF)) Some((code, raw.drop(4).take(raw.length - 6).utf8String))
			else None
		case _ ⇒ None
	}

	private def extractCode(b: ByteString): Option[Int] = {
		if (b.length >= 3 &&
		b(0).toChar.isDigit &&
		b(1).toChar.isDigit &&
		b(2).toChar.isDigit) Some(b.take(3).utf8String.toInt)
		else None
	}
}

sealed trait Command {
	val code: String
	val message: String
}
object Command {
	val CRLF = ByteString("\r\n")

	def apply(code: String, message: String): ByteString = {
		if (message.length > 0) ByteString(code + ": " + message + "\r\n")
		else ByteString(code + "\r\n")
	}

	def unapply(raw: ByteString): Option[(String, String)] = {
		extractCode(raw) match {
			case Some(code) ⇒
				if (raw.endsWith(CRLF)) Some((code, raw.drop(5).take(raw.length - 7).utf8String))
				else None
			case _ ⇒ None
		}
	}

	private def extractCode(b: ByteString): Option[String] = {
		if (b.length >= 4 &&
		b(0).toChar.isLetter && b(0).toChar.isLower &&
		b(1).toChar.isLetter && b(1).toChar.isLower &&
		b(2).toChar.isLetter && b(2).toChar.isLower &&
		b(3).toChar.isLetter && b(3).toChar.isLower &&
		b(4).toChar.isWhitespace) Some(b.take(4).utf8String)
		else None
	}
}