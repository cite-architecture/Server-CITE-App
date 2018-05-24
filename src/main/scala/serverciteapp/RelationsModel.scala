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
import edu.holycross.shot.citerelation._
import edu.holycross.shot.scm._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.RelationsModel")
object RelationsModel {
	val foundRelations = Vars.empty[CiteTriple]

	val allVerbs = Vars.empty[(Cite2Urn,String)]

	val urn = Var[Option[Urn]](None)
	val inputBoxUrnStr = Var("")
	val filterVerb = Var[Option[Cite2Urn]](None)
	val userMessageVisibility = Var("app_hidden")
	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null
	val userMessage = Var("")
	val userAlert = Var("default")

	case class HistoryItem(search:Urn, filter:Option[Cite2Urn]) {
		override def toString:String = {
			filter match {
				case Some(f) => s"${search} filtered by “${f.objectComponent}”."
				case None => s"${search}."
			}
		}
	}
	val searchHistory = Vars.empty[HistoryItem]

	def clearRelations:Unit = {
		urn.value = None
		filterVerb.value = None
		inputBoxUrnStr.value = ""
		foundRelations.value.clear
	}

	def updateHistory(hi:HistoryItem):Unit = {
		val oldHistory:Vector[HistoryItem] = {
			searchHistory.value.map(s => {
				s
			}).toVector

		}
		val newHistory:Vector[HistoryItem] = Vector(hi) ++ oldHistory
		searchHistory.value.clear
		for (i <- newHistory){
			searchHistory.value += i
		}
	}

	def getVerbLabel(u:Cite2Urn):String = {
		val testList = allVerbs.value.toVector
		testList.find(_._1 == u) match {
			case Some(l) => l._2
			case None => throw new Exception(s"Could not find label for ${u}.")
		}
	}


	/* gets list of all represented relations, if any.
	* Hands off to RelationsQuery; if there are no relatios, de-activate the tab.
	* If there are some, update the menu.
	*/
	def updateRelations:Unit = {
		RelationsModel.allVerbs.value.clear
		val task = Task{ CiteMainQuery.getJson(RelationsQuery.getVerbMap, RelationsQuery.queryVerbMap, urn = None) }
		val future = task.runAsync
	}

	def loadFoundRelations(u:Option[Urn], relations:Option[CiteRelationSet]):Unit = {
		relations match {
			case Some(crs) => {
				// Update history
				RelationsModel.filterVerb.value match {
					case Some(fv) => {
						val historyItem:RelationsModel.HistoryItem = RelationsModel.HistoryItem(RelationsModel.urn.value.get, Some(fv))
						RelationsModel.updateHistory(historyItem)	
					}
					case _ => {
						val historyItem:RelationsModel.HistoryItem = RelationsModel.HistoryItem(RelationsModel.urn.value.get, None )
						RelationsModel.updateHistory(historyItem)	
					}
				}	
				RelationsModel.foundRelations.value.clear
				val sortedRelations:Vector[CiteTriple] = crs.relations.toVector.sortBy(_.urn1.toString)
				for (r <- sortedRelations) {
					RelationsModel.foundRelations.value += r
				}
			}
			case None => {
				RelationsModel.foundRelations.value.clear
			}
		}
	}

	def loadVerbs(ol:Vector[(Cite2Urn,String)]):Unit = {
			allVerbs.value.clear
			// we'll try to improve the labels if we can
			for (v <- ol){
				val t:(Cite2Urn,String) = {
					ObjectModel.labelMap.value match {
						case Some(lm) => {
							if (lm.contains(v._1)){
								(v._1, lm(v._1))
							} else {
								v
							}
						}	
						case None => {
							v	
						}
					}	
				}
				allVerbs.value += t
			}
	}

}