

package org.gb.crm

/**
	* Created by neo on 30/10/16.
	*/
package object data {
	type UserRow = Tuple5[Int, String, String, Int, Int] // Id, Firstname, Lastname, Level, CompanyId
	type CompanyRow = Tuple5[Int, String, Int, String, Int] // Id, Name, Strength, Domain, Budget
	type SearchRow = Tuple8[Int, String, String, Int, String, Int, String, Int] // Id, domain, keyword1, weight1, keyword2, weight2, keyword3, weight3
	type EmailRow = Tuple5[Int, String, Int, Boolean, Boolean] //Id, Subject, CompanyId, Weight, WhitePaper, Promotional
	type WebsiteRow = Tuple5[Int, String, Int, Boolean, List[Int]] //Id, Page Name, Weight, hasHyperlink, List of page ids
}
