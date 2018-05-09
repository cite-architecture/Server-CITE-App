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
	val objectHistory = Vars.empty[(Cite2Urn, Int, Int)]

	// History item = URN, current offset, current limit
	def updateHistory(u:Cite2Urn, o:Int, l:Int):Unit = {
	    ObjectModel.objectHistory.value += Tuple3(u, o, l)
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
    val totalNumberOfObjects = Var(0)
	val boundObjects = Vars.empty[CiteObject]
	val boundDisplayObjects = Vars.empty[BoundDisplayObject]

	// for collection-browsing
	val isOrdered = Var(false)


	// For Display
	val offset = Var(0)
	val limit = Var(5)
	val totalReturnedUrns = Var(0)
	val showObjects = Var(false) // if true, show a whole object; false, URN+label
	val browsable = Var(false)
	val objectReport = Var("")

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
		// We don't want to reset the offset, since it is useful when moving from
		// an object to a collection
		//offset.value = 0 
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
		ObjectModel.updateHistory(u, ObjectModel.offset.value, ObjectModel.limit.value)	


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
					ObjectModel.loadNewCollectionUrns(u)
				} else {
					ObjectModel.setOffsetToCurrentUrn(u)
				}
			}
			case _ => {
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
		try {
			val offsetUrn:Cite2Urn = {
				if (u.isRange) {
					val rangeStart:Cite2Urn = rangeToTuple(u)._1	
					rangeStart
				} else {
					u
				}
			}
			offsetUrn.objectComponentOption match {
				case Some(oco) => {
					val newOffset:Int = boundCollectionUrns.value.zipWithIndex.filter(_._1 == offsetUrn)(0)._2
					ObjectModel.offset.value = newOffset
				}
				case None => // Do nothing
			}
		} catch {
			case e:Exception => s"Could not find offset for ${u}."
			ObjectController.updateUserMessage(s"Could not find offset for ${u}.",2)
		}
	}

	def constructBoundDisplayObject(obj:CiteObject):BoundDisplayObject = {
		try {
		  val collUrn:Cite2Urn = obj.urn.dropSelector
			val urn = Var(obj.urn)
			val label = Var(obj.label)
			val tempPropList = Vars.empty[BoundCiteProperty]
			for (p <- obj.propertyList){
					val tempU = Var(p.urn)
					val tempV = Var(p.propertyValue.toString)
					val tempT = Var(p.propertyDef.propertyType)
					val tempP = BoundCiteProperty(tempU,tempT,tempV)
					val props = Var(tempP)
					tempPropList.value += tempP
			}
			val tempProtocolList = Vars.empty[BoundCiteProtocol]

			val tempProt0 = Var(CiteMainModel.objectProtocol)
			tempProtocolList.value += BoundCiteProtocol(tempProt0)
			val tempBDO = BoundDisplayObject(urn,label,tempPropList,tempProtocolList)
			tempBDO
		} catch {
			case e:Exception => throw new Exception(s"ObjectModel.constructBoundDisplayObject failed for ${obj}")
		}
	}

	@dom
	def updatePrevNext:Unit = {
		ObjectView.cursorWaiting
		ObjectModel.objectOrCollection.value match {
			case "object" => {
				if (isOrdered.value) {
					val u:Option[Cite2Urn] = ObjectModel.urn.value
					u match {
						case Some(u) => {
							val prevQuery:String = ObjectQuery.queryGetPrevUrn + u.toString
							val nextQuery:String = ObjectQuery.queryGetNextUrn + u.toString
				    		val taskPrev = Task{ CiteMainQuery.getJson(ObjectQuery.getPrevUrn, prevQuery, urn = Some(u)) }
							val futurePrev = taskPrev.runAsync
				    		val taskNext = Task{ CiteMainQuery.getJson(ObjectQuery.getNextUrn, nextQuery, urn = Some(u)) }
							val futureNext = taskNext.runAsync
						}
						case None => {
							currentPrev.value = None
							currentNext.value = None
						}
					}
				} else {
					currentPrev.value = None
					currentNext.value = None
				}
			}
			case "none" => {
					currentPrev.value = None
					currentNext.value = None
					ObjectView.cursorNormal
			}
			case "search" => {
				val numC = totalNumberOfObjects.value
				if(limit.value >= numC){
					currentPrev.value = None
					currentNext.value = None
				} else {
					if ((offset.value + limit.value) > numC){
						currentNext.value = None
					} else {
						// get next
						val o:Int = offset.value + limit.value
						currentNext.value = Option(None,o,limit.value)
					}
					if (offset.value == 1 ){
						currentPrev.value = None
					} else {
						// get prev
						val o:Int = {
							if ((offset.value - limit.value) > 0){
								offset.value - limit.value
							} else { 0 }
						}
						//val u:Cite2Urn = objects.get(o).urn
						currentPrev.value = Option(None,o,limit.value)
					}
				}
				ObjectView.cursorNormal
			}
			case _ => {
				val numC = totalNumberOfObjects.value
				if(limit.value >= numC){
					currentPrev.value = None
					currentNext.value = None
					ObjectView.cursorNormal
				} else {
					if ((offset.value + limit.value) > numC){
						currentNext.value = None
					} else {
						// get next
						val o:Int = offset.value + limit.value
						currentNext.value = Option(urn.value,o,limit.value)
					}
					if (offset.value == 0 ){
						currentPrev.value = None
					} else {
						// get prev
						val o:Int = {
							if ((offset.value - limit.value) > 0){
								offset.value - limit.value
							} else { 0 }
						}
						//val u:Cite2Urn = objects.get(o).urn
						currentPrev.value = Option(urn.value,o,limit.value)
					}
					ObjectView.cursorNormal
				}
			}
		}
	}
	

	/* This is how to pass data to the global JS scope */
	/*
	js.Dynamic.global.currentObjectUrn = "urn:cts"
	js.Dynamic.global.roiArray = Array("one","two","three")
	*/

}
