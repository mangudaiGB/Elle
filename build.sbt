import sbtassembly.AssemblyKeys._

name := "predictive"

version := "1.0"

scalaVersion := "2.11.8"

val akkaVersion = "2.4.11"

libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-classic" % "1.1.7",
	"com.typesafe.akka" %% "akka-actor" % akkaVersion,
	"com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
	"com.typesafe.akka" %% "akka-stream" % akkaVersion
)

mainClass in Compile := Some("org.gb.crm.ApplicationServer")