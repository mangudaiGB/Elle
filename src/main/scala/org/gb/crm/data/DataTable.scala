
package org.gb.crm.data

import org.gb.crm.utils._


/**
	* Created by neo on 29/10/16.
	*/

trait BaseOps[A] {
	def table: Array[A]
	def insert(row: A): Array[A] = ???
	def insert(rows: Seq[A]): Array[A]
	def selectById(id: Long): Option[A]
}

class UserTable extends BaseOps[UserRow] {
	override def table: Array[UserRow] = Array()
	override def insert(rows: Seq[UserRow]): Array[UserRow] = table ++ rows
	override def selectById(id: Long): Option[UserRow] = table.filter(r ⇒ r._1 == id).headOption
}

class CompanyTable extends BaseOps[CompanyRow] {
	override def table: Array[CompanyRow] = Array()
	override def insert(rows: Seq[CompanyRow]): Array[CompanyRow] = table ++ rows
	override def selectById(id: Long): Option[CompanyRow] = table.filter(r ⇒ r._1 == id).headOption
}

class SearchTable extends BaseOps[SearchRow] {
	override def table: Array[SearchRow] = Array()
	override def insert(rows: Seq[SearchRow]): Array[SearchRow] = table ++ rows
	override def selectById(id: Long): Option[SearchRow] = table.filter(r ⇒ r._1 == id).headOption
	def selectBySearchString(search: String): Option[SearchRow] = table.filter(r ⇒ (r._3 equalsIgnoreCase search)
			|| (r._5 equalsIgnoreCase search) || (r._7 equalsIgnoreCase search)).headOption
}

class EmailTable extends BaseOps[EmailRow] {
	override def table: Array[EmailRow] = Array()
	override def insert(rows: Seq[EmailRow]): Array[EmailRow] = table ++ rows
	override def selectById(id: Long): Option[EmailRow] = table.filter(r ⇒ r._1 == id).headOption
}

class WebsiteTable extends BaseOps[WebsiteRow] {
	override def table: Array[WebsiteRow] = Array()
	override def insert(rows: Seq[WebsiteRow]): Array[WebsiteRow] = table ++ rows
	override def selectById(id: Long): Option[WebsiteRow] = table.filter(r ⇒ r._1 == id).headOption
}

object Table {

	def userTable: Array[UserRow] = {
		val uTable = new UserTable
		uTable.insert(NameGenerator.createAllUserNames)
	}

	def companyTable: Array[CompanyRow] = {
		val cTable = new CompanyTable
		cTable.insert(CompanyGenerator.Company)
	}

	def searchTable: Array[SearchRow] = {
		val sTable = new SearchTable
		sTable.insert(ContextGenerator.Search)
	}

	def emailTable: Array[EmailRow] = {
		val eTable = new EmailTable
		eTable.insert(EmailGenerator.Email)
	}

	def websiteTable: Array[WebsiteRow] = {
		val wTable = new WebsiteTable
		wTable.insert(WebsiteGenerator.Website)
	}
}

object TestTable {
	def main (args: Array[String] ): Unit = {
//		Table.userTable.map(println(_))
		Table.companyTable.map(println(_))
//		Table.searchTable.map(println(_))
	}
}