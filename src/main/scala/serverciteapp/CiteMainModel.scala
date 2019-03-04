package serverciteapp
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.concurrent
              .ExecutionContext
              .Implicits
              .global

import scala.scalajs.js
import scala.scalajs.js._
import edu.holycross.shot.cite._
import js.annotation._
import edu.holycross.shot.scm._


@JSExportTopLevel("CiteMainModel")
object CiteMainModel {

		val serviceUrl = Var("")

		val userMessage = Var("Main loaded.")
		val userAlert = Var("default")
	   val userMessageVisibility = Var("app_hidden")

	   val requestParameterUrn = Var[Option[Urn]](None)

		var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

		val currentLibraryMetadataString = Var("No library loaded.")

		// These are terrible and need to be replaced with real data model stuff
		val textProtocol:String = "TextProtocol"
		val objectProtocol:String = "ObjectProtocol"


		val showHmt = Var(true)
		val showTexts = Var(true)
		val showNg = Var(true)
		val showCollections = Var(true)
		val showImages = Var(true)
		val showRelations = Var(true)



}
