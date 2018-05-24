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
import edu.holycross.shot.citerelation._
import edu.holycross.shot.citejson._
import edu.holycross.shot.dse._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.RelationsQuery")
object RelationsQuery {


	val relationsJson:RelationsJson = RelationsJson()

	/* Get ObjectModel.labelMap */
	val queryVerbMap:String = "/relations/verbmap"

	def getVerbMap(jstring:String, urn:Option[Urn] = None):Unit = {
		val ol:scala.collection.immutable.Map[Cite2Urn,String] = CiteMainQuery.citeLibraryJson.parseLabelMap(jstring)
		ol.size match {
			case n if (n > 0) => {
				// Load list of verbs
				RelationsModel.loadVerbs(ol.toVector)	
				CiteMainModel.showRelations.value = true
			}
			case _ => {
			}
		}
	}

	def getRelations(jstring:String, urn:Option[Urn] = None):Unit = {
		val returnedRelations:Vector[CiteTriple] = relationsJson.parseVectorOfCiteTriples(jstring)
		// load relations
		val ors:Option[CiteRelationSet] = {
			returnedRelations.size match {
				case n if (n > 0) => Some(CiteRelationSet(returnedRelations.toSet))
				case _ => None
			}
		}
		RelationsModel.loadFoundRelations(urn, ors)
	}




}
