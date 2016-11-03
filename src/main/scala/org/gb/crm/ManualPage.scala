package org.gb.crm

/**
	* Created by neo on 29/10/16.
	*/
object ManPages {
	def apply(code: String): String = {
		code match {
			case "user" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                         USER MANUAL PAGE                                 |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  user [-l] [-c companyId] [-s selectionId]                               |
					|+--------------------------------------------------------------------------+
					||  user -l              : Displays a random list of 20 users.              |
					||                                                                          |
					||  user -c [companyId]  : Displays 10 users from the company               |
					||                                                                          |
					||  user -s [userId]     : Selects the user                                 |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "comp" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                     COMPANY  MANUAL PAGE                                 |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  comp [-l] [-s selectionId]                                              |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  comp -l             : List random 5 companies                           |
					||                                                                          |
					||  comp -s [sompanyId] : Selects the company for execution.                |
					||                                                                          |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "find" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                     SEARCH MANUAL PAGE                                   |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  find [-l] [-c] [-s keyword]                                             |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  find -l           :List all search keywords                             |
					||                                                                          |
					||  find -c           :Commit the changes and move to next stage.           |
					||                                                                          |
					||  find -s [keyword] :Select search keyword                                |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "mail" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                  E-MAIL MANUAL PAGE                                      |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||   mail [-l] [-c] [-s mailId]                                             |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  mail [-l]        : List all the mails for the user.                     |
					||                                                                          |
					||  mail [-c]        : Commit changes and move to next stage.               |
					||                                                                          |
					||  mail [-s]        : Select the email to process.                         |
					||                                                                          |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "site" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                 WEBSITE MANUAL PAGE                                      |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||   site [-l] [-c] [-s pageId]                                             |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  site [-l]             : List pages                                      |
					||                                                                          |
					||  site [-c]             : Commit changes                                  |
					||                                                                          |
					||  site [-s pageId]      : Select webpage.                                 |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "jump" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||          Welcome Mr Anderson! You have reached the Rabbit hole           |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||   jump [find] [mail] [site]                                              |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  jump [find]    : Jump to find stage. Run 'help mail' for more options   |
					||                                                                          |
					||  jump [mail]    : Jump to mail stage. Run 'help mail' for more options   |
					||                                                                          |
					||  jump [site]    : Jump to website stage. Run 'help site' for more options|
					||                                                                          |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case "pill" ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||          Welcome Mr Anderson! You have reached the Rabbit hole           |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||   pill [red] [blue]                                                      |
					|+--------------------------------------------------------------------------+
					||                                                                          |
					||  blue      : Cypher!!! ┻━┻ ︵ヽ(`Д´)ﾉ︵\uFEFF ┻━┻                         |
					||                                                                          |
					||  red       : Let's see how deep the rabbit hole goes.                    |
					||                                                                          |
					|+--------------------------------------------------------------------------+
				""".stripMargin
			case _ ⇒
				"""+--------------------------------------------------------------------------+
					||                                                                          |
					||                     Could not find a matching manual.                    |
					||                                                                          |
					|+--------------------------------------------------------------------------+
					||  Arguments for help can be:                                              |
					||                                                                          |
					||  user       : Help page for users.                                       |
					||                                                                          |
					||  comp       : Help page for company.                                     |
					||                                                                          |
					||  find       : Help page for search/find.                                 |
					||                                                                          |
					||  mail       : Help page for mail.                                        |
					||                                                                          |
					||  site       : Help page for site.                                        |
					||                                                                          |
					||  pill       : Help page for pill.                                        |
					|+--------------------------------------------------------------------------+
				""".stripMargin
		}
	}
}

object OutputPage {
	def apply: String =
		"""
			|+-------------------------------------------------------------------------+
			||
			||                            OUTPUT DATA
			|+-------------------------------------------------------------------------+
			||
			||  User: %1$s
			||
			||  Company: %2$s
			||  Domain:  %3$s                             Estimate: $ %4$s
			||  SearchValue    :  %5$s
			||  EmailValue     :  %6$s
			||  WebpageValue   :  %7$s
			||
			||  Cumulative Index :  %8$s
			|+-------------------------------------------------------------------------+
			||    Top Nearest Neighbour Scores
			||    1. %9$s
			||    2. %10$s
			||    3. %11$s
			||    4. %12$s
			||    5. %13$s
			|+-------------------------------------------------------------------------+
		""".stripMargin
}