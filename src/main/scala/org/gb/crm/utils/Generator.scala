

package org.gb.crm.utils


import java.io.{File, PrintWriter}

import scala.util.Random

/**
	* Created by neo on 29/10/16.
	*/

object NameGenerator {

	val fnsyllables: Array[String] = Array("mon", "shi", "sar", "lee", "sha", "sho", "rav", "roh", "ali",
		"pra", "ji", "ja", "bhi", "a", "izen", "rash", "vand", "sau", "thak", "sam", "zag", "fay", "hee", "na",
		"dim", "roh", "man", "deep", "ani", "abhi", "gau")

	val lnsyllables: Array[String] = Array("shu", "hant", "zex", "zak", "alv", "kau", "sah", "lun", "wei")

	def createName: String= {
		val firstname = (for ( i ← 1 to 2 + Random.nextInt(5)) yield Random.nextInt(31)).foldLeft("")(_ + fnsyllables(_))
		val lastname = (for ( i ← 1 to 2 + Random.nextInt(2)) yield Random.nextInt(9)).foldLeft("")(_ + fnsyllables(_))
		firstname + " " + lastname
	}

	def createFirstName: String = (for ( i ← 1 to 2 + Random.nextInt(5)) yield Random.nextInt(31)).foldLeft("")(_ + fnsyllables(_))

	def createLastName: String = (for ( i ← 1 to 2 + Random.nextInt(2)) yield Random.nextInt(9)).foldLeft("")(_ + fnsyllables(_))

	def createAllUserNames: Seq[(Int, String, String, Int, Int)] = {
		for (i ← 1 to 1000) yield (i, createFirstName, createLastName , 50 + Random.nextInt(20) ,1 +Random.nextInt(20))
	}

	def writeUserNameToFile = {
		val output = new PrintWriter(new File("usernames.csv"))
		for (i ← 1 to 1000) output.write(i + "," + createName + "," + (50 + Random.nextInt(30)).toString + "," +(1 +Random.nextInt(20)).toString + "\n")
		output.flush()
		output.close()
	}

	def main(args: Array[String]): Unit = {
//		writeUserNameToFile()
		val gb = createAllUserNames
		println(gb)
	}
}

object CompanyGenerator {
	def Company: Array[(Int, String, Int, String, Int)] = {
		Array(
			(1, "FadX LTD", 800, "Logistics", 120000),
			(2, "Amazin LLC", 500, "E-Commerce", 2000000),
			(3, "FlopKart LLC", 250, "E-Commerce", 800000),
			(4, "Appenture Services LTD", 1200, "Software Services", 7000000),
			(5, "Fork Motor Corporation", 600, "Auto-Mobile", 200000),
			(6, "Mistry Paloonji LTD", 200, "Construction", 500000),
			(7, "ICM LTD", 300, "Consumer Goods", 1000000),
			(8, "Team Please LTD", 45, "Data Analytics", 34000),
			(9, "Wini Corp", 500, "Technology", 200000),
			(10, "ONGL", 700, "Oil and Gas", 9000000),
			(11, "Separate LTD", 15, "Law", 7000000),
			(12, "UNIQ LLC", 1100, "Technology", 8000000),
			(13, "BENAM LTD", 350, "E-Commerce", 200000),
			(14, "UNLIKE LLC", 300, "Software Services", 3000000),
			(15, "BCMC LTD", 260, "Construction", 6000000),
			(16, "Timpu LLC", 210, "Electricity", 9000000),
			(17, "Extra Power LLC", 35, "Solar Power", 20000),
			(18, "Swagger IO LLC", 20, "Technology", 6000000),
			(19, "Axure LTD", 120, "Online Services", 3400000),
			(20, "Rocksette LLC", 300, "Entertainment", 4300000)
		)
	}
}

object ContextGenerator {
	def Search: Array[(Int, String, String, Int, String, Int, String, Int)] =
		Array(
			(1, "Logistics", "Move",	4,	"Relocate",	5,	"New Apartment",	2),
			(2, "E-Commerce",	"Online Sale",	4,	"Buy Online",	5,	"Amazin/Flopkart",	5),
			(3, "Software Services",	"Make website",	3,	"Build App",	2,	"Build service",	3),
			(4, "Auto-Mobile",	"Car",	5,	"Jeep",	5,	"Truck",	5),
			(5, "Construction",	"House",	5,	"Apartment",	3,	"Duplex",	2),
			(6, "Consumer Goods",	"Toothpaste",	4,	"laptop",	4,	"phone",	4),
			(7, "Data Analytics",	"Behaviour",	5,	"Insight",	5,	"Predict",	5),
			(8, "Technology",	"Protocol",	3,	"SDK",	5,	"API",	3),
			(9, "Oil and Gas",	"Petrol",	4,	"Diesel",	4,	"Kerosene",	4),
			(10, "Law",	"Sub-poena",	5,	"Court",	5,	"Sue",	5),
			(11, "Electricity",	"Battery",	4,	"Power",	2,	"Erratic",	3),
			(12, "Solar Power",	"Solar",	5,	"Eco-friendly",	2,	"Photo-Electric",	2),
			(13, "Online Services",	"Cloud",	5,	"Hosting",	2,	"Service",	2)
		)
}

// Id, subject, weight, hyperlink, attachment
object EmailGenerator {
	def Email: Array[(Int, String, Int, Boolean, Boolean)] =
		Array(
			(1, "A product which will solve your problems.", 1, false, false),
			(2, "The most awesome product you will ever find.", 1, false, false),
			(3, "A market survey about the product.", 2, false, false),
			(4, "Checkout the white paper for product.", 3, true, false),
			(5, "Checkout the trial for the product.", 4, false, true),
			(6, "White paper for product. Access the free trial", 5, true, true)
		)
}

object WebsiteGenerator {
	def Website: Array[(Int, String, Int, Boolean, List[Int])] =
		Array(
			(1, "Page 1", 1, true, List(2,3)),
			(2, "Page 2", 2, true, List(4,5)),
			(3, "Page 3", 3, true, List(6)),
			(4, "Page 4", 4, true, List(5)),
			(5, "Page 5", 5, false, List()),
			(6, "Page 6", 5, false, List())
		)
}