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

@JSExportTopLevel("QueryObjectQuery")
object QueryObjectQuery {


	val objJson:CiteObjJson = CiteObjJson()

	@dom
	def doQuery(jstring:String, urn:Option[Urn] = None):Unit = {
		val vco:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
		val stats = objJson.statsForVectorOfCiteObjects(jstring)
		stats match {
			case Some(s) => {
				ObjectModel.totalNumberOfObjects.value = s("total")
			}
			case None => {
				throw new Exception(s"Did not get valid stats on request from server.")
			}
		}
		val holdQuery:QueryObjectModel.CiteCollectionQuery = QueryObjectModel.currentQuery.value.get
		holdQuery.numResults = ObjectModel.totalNumberOfObjects.value
	    QueryObjectModel.currentQuery.value =  None
	    QueryObjectModel.currentQuery.value =  Some(holdQuery)
		QueryObjectController.loadSearchResults(vco)
		ObjectView.cursorNormal
	}
	

}
