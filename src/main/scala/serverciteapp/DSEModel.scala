package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import js.annotation._
import scala.concurrent._
//import ExecutionContext.Implicits.global
import collection.mutable
import collection.mutable._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.DSEModel")
object DSEModel {

	val dseModelUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:dse")
	val dseImageProp:String = "imageroi"
	val dseTextProp:String = "passage"
	val dseSurfaceProp:String = "surface"

	val dsesForObjectProperties = Vars.empty[DseRecord]
	val dsesForTexts = Vars.empty[DseRecord]
	val dsesForImage = Vars.empty[DseRecord]

	def clearDsesForObjectProperties:Unit = dsesForObjectProperties.value.clear
	def clearDsesForTexts:Unit = dsesForTexts.value.clear
	def clearDsesForImages:Unit = dsesForImage.value.clear

	def updateDsesForText(u:CtsUrn) = {
		g.console.log("Clearing DSEs for text.")
		clearDsesForTexts	
		g.console.log(s"Will update DSEs for: ${u}")
	}

}
