enablePlugins(ScalaJSPlugin, BuildInfoPlugin)

name := "serverciteapp"

version := "1.3.0"

scalaVersion := "2.12.6"


resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")

val circeVersion = "0.9.0"

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
  "org.scala-js" %%% "scalajs-dom" % "0.9.5",
  "io.monix" %%% "monix" % "2.3.0",
  "edu.holycross.shot.cite" %%% "xcite" % "3.3.0",
  "edu.holycross.shot" %%% "ohco2" % "10.7.1" from "file:////Users/cblackwell/Dropbox/CITE/scala/unmanaged_jars/ohco2_sjs0.6_2.12-10.7.1.jar",
  "edu.holycross.shot" %%% "scm" % "6.0.0",
  "edu.holycross.shot" %%% "citeobj" % "7.0.1",
  "edu.holycross.shot" %%% "dse" % "3.0.0",
  "edu.holycross.shot" %%% "citerelations" % "2.0.4",
  "edu.holycross.shot" %%% "citebinaryimage" % "1.1.2",
  //"edu.holycross.shot" %%% "citejson" % "2.0.0" from "file:///cite/scala/unmanaged_jars/citejson_sjs0.6_2.12-2.0.0.jar",
  "edu.holycross.shot" %%% "citejson" % "2.5.0" from "file:////Users/cblackwell/Dropbox/CITE/scala/unmanaged_jars/citejson_sjs0.6_2.12-2.5.0.jar",
  "com.thoughtworks.binding" %%% "dom" % "11.0.1",
)
libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core",
  "io.circe" %%% "circe-generic",
  "io.circe" %%% "circe-optics",
  "io.circe" %%% "circe-parser"
).map(_ % circeVersion)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

//scalacOptions += "-P:scalajs:suppressExportDeprecations"
//scalacOptions += "-P:scalajs:suppressMissingJSGlobalDeprecations"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

lazy val spa = taskKey[Unit]("Assemble single-page app from html templates and generated CSS and JS output")

import scala.io.Source
import java.io.PrintWriter
spa := {

//	val defaultLibraryUrl = "https://raw.githubusercontent.com/cite-architecture/citedx/master/libraries/millionplus.cex"
  val defaultServiceUrl = "http://beta.hpcc.uh.edu/scs"
//  val defaultServiceUrl = "http://localhost:9000"

  val compileFirst = (fullOptJS in Compile).value

  val junk = "//# sourceMappingURL=serverciteapp-opt.js.map"
  val js = Source.fromFile("target/scala-2.12/serverciteapp-opt.js").getLines.mkString("\n").replaceAll(junk,"")

  val css = Source.fromFile("target/scala-2.12/classes/application.css").getLines.mkString("\n")

  val template1 = "src/main/resources/cite-TEMPLATE1.html"
  val template1Text = Source.fromFile(template1).getLines.mkString("\n").replaceAll("ACTUALVERSION", version.value).replaceAll("ACTUALCSS",css)


	val urlPlaceholder = "DEFAULTSERVICEURL"
	val delimiterPlaceholder = "DEFAULTLIBRARYDELIMITER"

  val template2Text = Source.fromFile("src/main/resources/cite-TEMPLATE2.html").getLines.mkString("\n").replaceAll(urlPlaceholder,defaultServiceUrl)
  val newFile = "downloads/cite-" + version.value + ".html"
  new PrintWriter(newFile) { write(template1Text + js + template2Text); close }
  println("Runnable single-page app is in " + newFile)
}

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "serverciteapp"
