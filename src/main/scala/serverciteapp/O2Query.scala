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
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citejson._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.O2Query")
object O2Query {


	val o2Json:Ohco2Json = Ohco2Json()

	/* Queries */
	val queryCatalog:String = "/texts"

	def updateCatalog(jstring:String):Unit = {
		val cat:Catalog = o2Json.o2Catalog(jstring)
		O2Model.currentCatalog.value = {
			cat.size match {
				case n if (n > 0) => Some(cat)
				case _ => None
			}
		} 
		O2Model.currentCatalog.value match {
			case Some(cat) => {
				O2Model.citedWorks.value.clear
				for ( cw <- cat.urnList){
					O2Model.citedWorks.value += cw
				}
				CiteMainModel.showTexts.value = true
				O2Controller.updateUserMessage(s"Updated text repository with ${O2Model.citedWorks.value.size} versions of works.",0)
				CiteMainView.changeTab("text")
				O2Controller.preloadUrn
			}
			case None => {
				O2Model.citedWorks.value.clear
				CiteMainModel.showTexts.value = false
			}
		}
	}

	val queryFirstUrn:String = "/texts/firsturn/"

	def getFirstUrn(jstring:String):Unit = {
		val urn:CtsUrn = o2Json.o2CtsUrnString(jstring)
		O2Model.urn.value = urn
		O2Controller.validUrnInField.value = true
	}

	def getFirstNodeUrn(jstring:String):Unit = {
		val urn:CtsUrn = o2Json.o2CtsUrnString(jstring)
		js.Dynamic.global.document.getElementById("o2_urnInput").value = urn.toString
		O2Controller.validUrnInField.value = true
	}



}
