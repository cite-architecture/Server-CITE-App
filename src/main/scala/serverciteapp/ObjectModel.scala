package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.Dynamic.{ global => g }
import js.annotation._
import collection.mutable
import collection.mutable._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.concurrent._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("serverciteapp.ObjectModel")
object ObjectModel {

	// Binding up objects, their properties, and extensions

	case class BoundCiteProperty(urn:Var[Cite2Urn],propertyType:Var[CitePropertyType],propertyValue:Var[String])

	case class BoundCiteProtocol(prot:Var[String])

    case class BoundDisplayObject(urn:Var[Cite2Urn], label:Var[String],props:Vars[BoundCiteProperty], prot:Vars[BoundCiteProtocol])

	// Keeping track of the history of object-viewing
	val objectHistory = Vars.empty[Cite2Urn]

	def updateHistory(u:Cite2Urn):Unit = {
		if (ObjectModel.objectHistory.value.contains(u) == false)  {
	    	ObjectModel.objectHistory.value += u
		}		
	}
	// Messages
	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null
	val userMessage = Var("")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")


	// urn is what the user requested
	val urn = Var[Option[Cite2Urn]](None)
	// displayUrn is what will be shown
	val displayUrn = Var[Option[Cite2Urn]](None)

	// Keeping track of current collection data
	val collections = Vars.empty[CiteCollectionDef]
	val currentCatalog = Var[Option[CiteCatalog]](None)
	val hasCollections = Var(false)
	val labelMap = Var[Option[scala.collection.immutable.Map[Cite2Urn,String]]](None)

    val boundCollectionUrns = Vars.empty[Cite2Urn]
	val boundObjects = Vars.empty[CiteObject]
	val boundDisplayObjects = Vars.empty[BoundDisplayObject]

	// for collection-browsing
	val isOrdered = Var(false)


	// For Display
	val offset = Var(0)
	val limit = Var(5)
	val showObjects = Var(false) // if true, show a whole object; false, URN+label
	val browsable = Var(false)
	val objectReport = Var("")
	// Note the "justBrowsing" flag. We don't want to save every "prev" or "next" click in the History menu… only the first one and last one of a browsing session.
	var justBrowsing:Boolean = false

	// for navigation
	//      the prevOption and nextOption params are:
	//		Urn, offset, limit
	val prevOption:Option[Tuple3[Option[Cite2Urn],Int,Int]] = None
	val nextOption:Option[Tuple3[Option[Cite2Urn],Int,Int]] = None
	val currentPrev = Var(prevOption)
	val currentNext = Var(nextOption)

	// Object-or-collection? (based on current request)
	//    Choices: "none","object","collection","range","search"
	val objectOrCollection = Var("none")


	// Clears all current object data, and with it, displayed objects
	@dom
	def clearObject:Unit = {
		boundObjects.value.clear
		boundDisplayObjects.value.clear
		urn.value = None
		browsable.value = false
		currentPrev.value = None
		currentNext.value = None
		objectReport.value = ""
	offset.value = 0 
	}

	def updateCollections:Unit = {
		ObjectModel.collections.value.clear
		val task = Task{ CiteMainQuery.getJson(ObjectQuery.updateCatalog, ObjectQuery.queryCatalog, urn = None) }
		val future = task.runAsync
	}

	def updateLabelMap:Unit = {
		ObjectModel.labelMap.value = None
		val task = Task{ CiteMainQuery.getJson(ObjectQuery.getLabelMap, ObjectQuery.queryLabelMap, urn = None) }
		val future = task.runAsync
	}

	@dom
	def countObjectsInCollection(urn:Cite2Urn) = {
		ObjectModel.labelMap.bind match {
			case Some(lm) => {
				lm.filterKeys( _ ~~ urn ).size
			}
			case None => 0
		}
	}

	// Returns two urns representing the ends of a range of objects in an ordered collection
	def rangeToTuple(u:Cite2Urn):Tuple2[Cite2Urn,Cite2Urn] = {
		val rb:String = u.rangeBegin
		val re:String = u.rangeEnd
		val coll:String = u.dropSelector.toString
		val rbU:Cite2Urn = Cite2Urn(s"${coll}${rb}")
		val reU:Cite2Urn = Cite2Urn(s"${coll}${re}")
		val rangeTuple:Tuple2[Cite2Urn,Cite2Urn] = (rbU,reU)
		rangeTuple
	}

	/* Given a URN, gets…
		- the URNs for the whole collection
		- for a single Object, that object
			- stashing its current offset in its collection
		- for a range, up to [defaultLimit] objects, with paging for more
		- for a Collection, [defaultLimit] objects, with paging
	We use the ObjectModel.urn bound value to make decisions about paging.

	*/
	def getObjects(u:Cite2Urn):Unit = {
		ObjectView.cursorWaiting
		// Stash this in the history
		if (ObjectModel.justBrowsing != true ) {
			ObjectModel.updateHistory(u)	
			ObjectModel.justBrowsing = true
		} 


		if (u.isRange){
			if (ObjectModel.currentCatalog.value.get.isOrdered(u.dropSelector)){
				val qs:String = s"${ObjectQuery.queryGetPaged}${u}?offset=${offset.value}&limit=${limit.value}"
	    		val task = Task{ CiteMainQuery.getJson(ObjectQuery.getRangeOrCollection, qs, urn = Some(u)) }
	    		val future = task.runAsync
			} else {
				ObjectController.updateUserMessage(s"The collection ${u.dropSelector} is not an ordered collection, so range-citations are not applicable.",2)
			}
		} else {
			u.objectComponentOption match {
				// Just object
				case Some(o) => {
					val qs:String = s"${ObjectQuery.queryGetObjects}${u}"
		    		val task = Task{ CiteMainQuery.getJson(ObjectQuery.getObject, qs, urn = Some(u)) }
					val future = task.runAsync
				}
				// collection
				case None => {
					val qs:String = s"${ObjectQuery.queryGetPaged}${u}?offset=${offset.value}&limit=${limit.value}"
		    		val task = Task{ CiteMainQuery.getJson(ObjectQuery.getRangeOrCollection, qs, urn = Some(u)) }
					val future = task.runAsync
				}
			}
		}
	}

	def collectionUrnCheck(u:Cite2Urn):Unit = {	
		// Check to see if we need to reload Collection URNs… if so, go ahead and get on that!
		val collUrn:Cite2Urn = u.dropSelector
		ObjectModel.boundCollectionUrns.value.size match {
			case n if (n > 0) => {
				val cu = ObjectModel.boundCollectionUrns.value
				if (cu(0).dropSelector != collUrn) {
					g.console.log(s"${cu(0)} != ${collUrn}: reloading boundCollectionUrns")
					ObjectModel.loadNewCollectionUrns(u)
				} else {
					ObjectModel.setOffsetToCurrentUrn(u)
				}
			}
			case _ => {
				g.console.log(s"No boundCollectionUrns: reloading")
				ObjectModel.loadNewCollectionUrns(u)
			}
		}	
	}

	// We go ahead and reset offset to 0 here, since we've loaded a new collection
	def loadNewCollectionUrns(u:Cite2Urn):Unit = {
		val collUrn:Cite2Urn = u.dropSelector
		val qs:String = s"${ObjectQuery.queryGetCollectionUrns}${collUrn}"
		val task = Task{ CiteMainQuery.getJson(ObjectQuery.getCollectionUrns, qs, urn = Some(u)) }
		val future = task.runAsync
	}

	def setOffsetToCurrentUrn(u:Cite2Urn):Unit = {
		g.console.log(s"Need to set offset for ${u}")
	}

	/* This is how to pass data to the global JS scope */
	/*
	js.Dynamic.global.currentObjectUrn = "urn:cts"
	js.Dynamic.global.roiArray = Array("one","two","three")
	*/

}
