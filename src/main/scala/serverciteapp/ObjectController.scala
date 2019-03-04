package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import js.annotation._
import collection.mutable
import collection.mutable._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.concurrent._
//import ExecutionContext.Implicits.global
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("ObjectController")
object ObjectController {

	def objectIsPresent(u:Cite2Urn):Boolean = {
		val tempU:Cite2Urn = u.dropExtensions
		ObjectModel.labelMap.value match {
			case Some(lm) => {
				(ObjectModel.labelMap.value.get(tempU).size > 0)
			}
			case None => false
		}
	}

	def labelForCollection(u:Cite2Urn):String = {
		val collUrn = u.dropSelector
		ObjectModel.currentCatalog.value match {
			case Some(cc) => {
				cc.collection(collUrn) match {
					case Some(cd) => cd.collectionLabel
					case None => s"Collection ${collUrn} is not represented in this repository."
				}
			}		
			case None => s"Collection ${collUrn} is not represented in this repository."
		}
	}

	def updateUserMessage(msg: String, alert: Int): Unit = {
		ObjectModel.userMessageVisibility.value = "app_visible"
		ObjectModel.userMessage.value = msg
		alert match {
			case 0 => ObjectModel.userAlert.value = "default"
			case 1 => ObjectModel.userAlert.value = "wait"
			case 2 => ObjectModel.userAlert.value = "warn"
		}
		js.timers.clearTimeout(ObjectModel.msgTimer)
		ObjectModel.msgTimer = js.timers.setTimeout(6000){ ObjectModel.userMessageVisibility.value = "app_hidden" }
	}

	// On key-up, and on submit, checks for valid Cite2Urn, and decides
	// whether the request is for an object, or a range, or nothing
	def validateUrn(urnString: String): Unit = {
		try{
			val newUrn: Cite2Urn = Cite2Urn(urnString)
			newUrn.objectComponentOption match {
				case Some(o) => {
					newUrn.rangeBeginOption match {
						case Some(rb) => {
								newUrn.rangeEndOption match {
									case Some (re) => {
										ObjectModel.objectOrCollection.value = "range"
									}
									case _ => {
										ObjectModel.objectOrCollection.value = "none"
									}
								}
						}
						case _ => {
							ObjectModel.objectOrCollection.value = "object"
						}
					}
				}
				case _ => {
					ObjectModel.objectOrCollection.value = "collection"
				}
			}
		} catch {
			case e: Exception => {
				ObjectModel.objectOrCollection.value = "none"
			}
		}
	}

	def updateReport:Unit = {
		val collUrn:Cite2Urn = ObjectModel.urn.value.get.dropSelector
		val collLabel:String = ObjectModel.currentCatalog.value.get.collection(collUrn).get.collectionLabel
		val n:Int = ObjectModel.boundObjects.value.size
		val total:Int = ObjectModel.totalNumberOfObjects.value
		val report = s"Showing ${n} out of ${total} objects in collection: ${collLabel} [${collUrn}]."
		ObjectModel.objectReport.value = report
	}


	/* I don't know how to do anything equivalent to "this" when passing events
	in ScalaJS. So this will cover all numeric fields. */
	def validateNumericEntry(thisEvent: Event):Unit = {
		val oldOffset:Int = ObjectModel.offset.value
		val oldLimit:Int = ObjectModel.limit.value
		val thisTarget = thisEvent.target.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
		val targetId = thisTarget.id
		val testText = thisTarget.value.toString
		try{
			val mo: Int = testText.toInt
			targetId match {
				case "object_browseOffset" => ObjectModel.offset.value = mo
				case "object_browseLimit" => ObjectModel.limit.value = mo
			}
		} catch {
			case e: Exception => {
				val badMo: String = testText
				ObjectModel.offset.value = oldOffset
				ObjectModel.limit.value = oldLimit
				targetId match {
					case "object_browseOffset" => {
						ObjectController.updateUserMessage(s"Offset value must be an integer. '${badMo}' is not an integer.", 2)
						thisTarget.value =  ObjectModel.offset.value.toString
					}
					case "object_browseLimit" => {
						ObjectController.updateUserMessage(s"Limit value must be an integer. '${badMo}' is not an integer.", 2)
						thisTarget.value =  ObjectModel.limit.value.toString
					}
				}

			}
		}
	}

	def preloadUrn:Unit = {
		// get first collection in catalog
		if (
			(ObjectModel.collections.value.size > 0) &
			(ObjectModel.labelMap.value != None )
		){
			val collUrn = ObjectModel.collections.value(0).urn
			insertFirstObjectUrn(collUrn)
		} else {

		}
	}

	def insertFirstObjectUrn(urn: Cite2Urn): Unit = {
		ObjectView.cursorWaiting
		val offset:Int = 0
		val limit:Int = 1
		val qs:String = s"${ObjectQuery.queryPagedObjects}${urn}?offset=${offset}&limit=${limit}"
		val task = Task{ CiteMainQuery.getJson(ObjectQuery.doInsertFirstObjectUrn, qs, urn = Some(urn)) }
		val future = task.runAsync
	}

	// Based on the UI toggle, sets showObject.
	//     true -> show each object and all its properties
	//     false -> show URN and label only
	@dom
	def switchDisplay(thisEvent: Event):Unit = {
		ObjectView.cursorWaiting
		val before = ObjectModel.showObjects.value
		val showObjectsStr:String = js.Dynamic.global.document.getElementById("object_browseOrListSwitch").checked.toString
		ObjectModel.showObjects.value = (showObjectsStr == "true")
		ObjectController.setDisplay
		ObjectView.cursorNormal
	}


	@dom
	def getPrev:Unit = {
		ObjectModel.currentPrev.value match {
			case Some(u) => {
				val no:Int = u._2
				val nl:Int = u._3
				ObjectModel.objectOrCollection.value match {
					case "object" => {
						u._1 match {
							case Some(cu) => {
								ObjectController.changeUrn(cu)
							}
							case _ => ObjectController.updateUserMessage("The URN for the previous object is None. This is an error. Please file an issue on GitHub.",2)
						}
					}
					case "none" => {
						ObjectController.updateUserMessage("There is no object. getPrev should not have been called",2)
					}
					case "search" => {
						// Duplicate current Query with new offset and limit
						val nq:QueryObjectModel.CiteCollectionQuery = QueryObjectModel.currentQuery.value.get.updatePosition(no, nl)
						QueryObjectModel.currentQuery.value = Some(nq)
						QueryObjectController.loadQuery(nq, no, nl)
					}
					case _ => {
						ObjectModel.limit.value = nl
						ObjectModel.offset.value = no
						ObjectController.changeObject
					}
				}
			}
			case _ => {
					ObjectController.updateUserMessage("There is no previous object. getPrev should not have been called",2)
			}
		}
	}
	
	@dom
	def getNext:Unit = {
		ObjectModel.currentNext.value match {
			case Some(u) => {
				val no:Int = u._2
				val nl:Int = u._3
				ObjectModel.objectOrCollection.value match {
					case "object" => {
						u._1 match {
							case Some(cu) => {
								ObjectController.changeUrn(cu)
							}
							case _ => ObjectController.updateUserMessage("The URN for the next object is None. This is an error. Please file an issue on GitHub.",2)
						}
					}
					case "none" => {
						ObjectController.updateUserMessage("There is no object. getNext should not have been called. Please file an issue on GitHub.",2)
					}
					case "search" => {
						// Duplicate current Query with new offset and limit
						val nq:QueryObjectModel.CiteCollectionQuery = QueryObjectModel.currentQuery.value.get.updatePosition(no, nl)
						QueryObjectModel.currentQuery.value = Some(nq)
						QueryObjectController.loadQuery(nq, no, nl)
					}
					// range, search results, or paged collection
					case _ => {
						ObjectModel.limit.value = nl
						ObjectModel.offset.value = no
						ObjectController.changeObject
					}
				}
			}
			case _ => {
					ObjectController.updateUserMessage("There is no next object. getNext should not have been called. Please file an issue on GitHub.",2)
			}
		}
	}

	def changeUrn(urnStr:String): Unit = {
		try {
			val u:Cite2Urn = Cite2Urn(urnStr)
			changeUrn(u)
		} catch {
			case e:Exception => throw new Exception(s"${urnStr} is not a valid Cite2 Urn.")
		}
	}

	def changeUrn(urn: Cite2Urn): Unit = {
		try {
			if (ObjectModel.hasCollections.value ){
				ObjectModel.urn.value = Some(urn)
				val collUrn = urn.dropSelector
				ObjectModel.displayUrn.value = Some(urn)
				ObjectModel.urn.value.get.objectComponentOption match {
					case Some(o) => {
						// test for range
						ObjectModel.urn.value.get.rangeBeginOption match {
							case Some(rb) => {
								ObjectModel.urn.value.get.rangeEndOption match {
									case Some(re) => {
										ObjectModel.offset.value = 0
										ObjectModel.objectOrCollection.value = "range"
										ObjectModel.isOrdered.value = ObjectModel.currentCatalog.value.get.isOrdered(collUrn)
										ObjectController.updateUserMessage("Retrieving range…",1)
									}
									case _ => {
										ObjectModel.objectOrCollection.value = "none"
										ObjectModel.isOrdered.value = false
									}
								}
							}
							// if not a range, it is an object
							case _ =>{
								ObjectModel.objectOrCollection.value = "object"
								ObjectModel.isOrdered.value = ObjectModel.currentCatalog.value.get.isOrdered(collUrn)
								ObjectController.updateUserMessage("Retrieving object…",1)
							}
						}
					}
					// otherwise, this is a collection
					case _ => {
						ObjectModel.objectOrCollection.value = "collection"
						ObjectModel.isOrdered.value = ObjectModel.currentCatalog.value.get.isOrdered(collUrn)
						ObjectController.updateUserMessage("Retrieving collection…",1)
					}
				}
				if (ObjectModel.objectOrCollection.value != "none") {
					val task = Task{ ObjectController.changeObject }
					val future = task.runAsync
				}
			}
		} catch {
			case e: Exception => {
				ObjectModel.objectOrCollection.value = "none"
				ObjectModel.isOrdered.value = false
				updateUserMessage(s"Invalid URN. Current URN not changed. ${e}",2)
			}
		}
	}

	// This hands off to ObjectModel.getObjects
	def changeObject:Unit = {
		if (ObjectModel.hasCollections.value) {
			val tempUrn:Cite2Urn = ObjectModel.urn.value.get
			ObjectModel.clearObject
			QueryObjectModel.clearAll
			ObjectModel.urn.value = Some(tempUrn)
			val collUrn = ObjectModel.urn.value.get.dropSelector

			// Based on the new URN, set image, ordered, browsable flags
			ObjectModel.isOrdered.value = ObjectModel.currentCatalog.value.get.isOrdered(collUrn)

			if (
					(ObjectModel.objectOrCollection.value == "collection") ||
					(ObjectModel.urn.value.get.isRange == true) ||
					(ObjectModel.isOrdered.value == true) ||
					(ObjectModel.urn.value.get.objectOption == None)
				){
				  	ObjectModel.browsable.value = true
				} else { ObjectModel.browsable.value = false }

				ObjectModel.objectOrCollection.value match {
						case "object" => {
							ObjectModel.getObjects(tempUrn)
						}
						case "collection" =>{
							ObjectModel.getObjects(tempUrn)
						}
						case "range" =>{
							ObjectModel.getObjects(tempUrn)
						}
						case _ => {
						}
				}
		}
	}

	def setDisplay: Unit = {
		val numObj:Int = ObjectModel.totalNumberOfObjects.value
		val tLim:Int = ObjectModel.limit.value
		val tOff:Int = ObjectModel.offset.value
		/*
		val startIndex:Int = tOff - 1
		val endIndex:Int = {
			if ( (tOff + tLim - 1)  >= numObj ) {
				(numObj - 1)
			} else {
				((tOff - 1) + (tLim - 1))
			}
		}
		*/
		ObjectModel.objectOrCollection.value match {
			case "object" => {
				ObjectModel.boundDisplayObjects.value.clear
				ObjectModel.boundDisplayObjects.value += ObjectModel.constructBoundDisplayObject(ObjectModel.boundObjects.value(0))
				ObjectModel.updatePrevNext
				ObjectController.updateReport
			}
			case "search" => {
				ObjectModel.boundDisplayObjects.value.clear
				for (i <- ObjectModel.boundObjects.value){
					ObjectModel.boundDisplayObjects.value += ObjectModel.constructBoundDisplayObject(i)
				}
				ObjectModel.updatePrevNext
			}
			case _ => {
				try {
					ObjectModel.urn.value match {
						case Some(cu) => {
							val collUrn:Cite2Urn = cu.dropSelector
							ObjectModel.boundDisplayObjects.value.clear
							for (i <- ObjectModel.boundObjects.value){
								ObjectModel.boundDisplayObjects.value += ObjectModel.constructBoundDisplayObject(i)
							}
							ObjectModel.updatePrevNext
							ObjectController.updateReport
						} 
						case None => ObjectController.updateUserMessage(s"Value 'urn' for ObjectModel is not set: ${ObjectModel.urn.value}.",1)
					}
				} catch {
					case e: Exception => {
						ObjectController.updateUserMessage(s"Failed on setDisplay. ${e}",2 )
					}

				}
			}
		}
	}


// below is how you invoke a cofirmation dialog
// val cc = window.confirm("Hi")
//CiteLinks.switch(urnSt)


}
