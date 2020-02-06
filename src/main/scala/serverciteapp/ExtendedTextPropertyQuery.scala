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
import edu.holycross.shot.citejson._
import edu.holycross.shot.scm._

import monix.execution.Scheduler.Implicits.global
import monix.eval._
import com.karasiq.highlightjs.HighlightJS
import com.karasiq.markedjs.{Marked, MarkedOptions, MarkedRenderer}
import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("ExtendedTextPropertyQuery")
object ExtendedTextPropertyQuery {

	val objJson:CiteObjJson = CiteObjJson()

	/* Does the work to build a list of extended cite-properties.
		 Value saved as DataModelModel.extendedTextProperties
	*/
	def getExtendedTextProperties: Unit = {
		val extendedTextDM = Cite2Urn("urn:cite2:cite:datamodels.v1:extensions_text")
		DataModelModel.dataModels.value match {
			case None => // do nothing
			case Some(dmv) => {
				val colls: Vector[Cite2Urn] = dmv.filter(_.model == extendedTextDM).map(_.collection)
				// Async, so we set it up here, and finish elsewhere…
				val collString = colls.map(u => {
					s"urn=${u}"
				}).mkString("&")
				val qs:String = s"/collections/objects?${collString}"
				val task = Task{ CiteMainQuery.getJson(getExtendedTextProperties2, qs, urn = None) }
				val future = task.runAsync
				// do nothing
			}
		}
	}

	def getExtendedTextProperties2(jstring:String, urn:Option[Urn] = None): Unit = {
		val vco:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
		val extProps: Option[Vector[(Cite2Urn, String)]] = {
			if (vco.size < 1) {
				None
			} else {
				val vec: Vector[(Cite2Urn, String)] = {
					vco.map( o => {
				    val pu = o.urn.addProperty("propertyurn")
				    val su = o.urn.addProperty("extendedtype")
				    (o.propertyValue(pu).asInstanceOf[Cite2Urn], o.propertyValue(su).asInstanceOf[String])
					})
				}
				Some(vec)
			}
		}

		// Save these
		DataModelModel.extendedTextProperties.value = extProps
	}
	
}