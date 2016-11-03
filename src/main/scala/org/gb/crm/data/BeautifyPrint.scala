package org.gb.crm.data

/**
	* Created by neo on 30/10/16.
	*/

trait BeautifyPrint {
	def PrintUser: Seq[Any] ⇒ String = "%5s %30s %20s %10s %10s".format
	def PrintCompany: Seq[Any] ⇒ String = "%3s %40s %10s %25s %10s".format
	def PrintSearch: Seq[Any] ⇒ String = "%3s %25s %20s %5s %20s %5s %20s %5s".format
	def PrintEmail: Seq[Any] ⇒ String = "%3s| %50s| %10s| %10s".format
	def PrintWebsite: Seq[Any] ⇒ String = "%3s %30s %10s %20s".format
}

object BeautifyPrint extends BeautifyPrint
