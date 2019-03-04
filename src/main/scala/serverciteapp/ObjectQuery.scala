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
import edu.holycross.shot.dse._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("ObjectQuery")
object ObjectQuery {


	val objJson:CiteObjJson = CiteObjJson()

	/* This does a lot of work:
		- It sees if there is a CollectionRepository
		- If so, it lets ObojectModel know
		- It shows the Collection tab
		- It updates the labelMap, which in turn preloads the first collection's first URN
	*/
	val queryCatalog:String = "/collections/"

	def updateCatalog(jstring:String, urn:Option[Urn] = None):Unit = {
		val cat:CiteCatalog = objJson.citeCatalog(jstring)
		ObjectModel.currentCatalog.value = {
			cat.size match {
				case n if (n > 0) => Some(cat)
				case _ => None
			}
		} 
		ObjectModel.currentCatalog.value match {
			case Some(cat) => {
				ObjectModel.hasCollections.value = true
				ObjectModel.collections.value.clear
				for ( coll <- cat.collections){
					ObjectModel.collections.value += coll
				}

				CiteMainModel.showCollections.value = true

				ObjectModel.updateLabelMap
				ObjectController.updateUserMessage(s"Updated collections repository with ${ObjectModel.collections.value.size} collections.",0)

				// Load request parameter
				CiteMainModel.requestParameterUrn.value match {
					case Some(u) => {
						u match {
							case Cite2Urn(c2u) => {
								DataModelController.retrieveObject(None, Cite2Urn(c2u))
								CiteMainView.changeTab("object")
							}
							case _ => // do nothing
						}
					}	
					case None => // do nothing
				}
			}				
			case None => {
				ObjectModel.collections.value.clear
				ObjectModel.hasCollections.value = false
				CiteMainModel.showCollections.value = false
			}
		}
	}

	/* Get ObjectModel.labelMap */
	val queryLabelMap:String = "/collections/labelmap"

	def getLabelMap(jstring:String, urn:Option[Urn] = None):Unit = {
		val ol:scala.collection.immutable.Map[Cite2Urn,String] = CiteMainQuery.citeLibraryJson.parseLabelMap(jstring)
		ol.size match {
			case n if (n > 0) => {
				ObjectModel.labelMap.value = Some(ol)
				ObjectController.preloadUrn
				// Improve Relations-labels, if appropriate
				if (RelationsModel.allVerbs.value.size > 0) {
					RelationsModel.loadVerbs(RelationsModel.allVerbs.value.toVector)
				}

			}
			case _ => {
				ObjectModel.labelMap.value = None	
			}
		}
	}

	/* Get Paged Objects */
	/* Also useful for getting the first object */

	val queryPagedObjects:String = "/objects/paged/"

	def doInsertFirstObjectUrn(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
			val vco:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
			val obj:CiteObject = {
				vco size match {
					case n if (n > 0) => vco(0)
					case _ => throw new Exception(s"${urn} yielded no object.")
				}
			}
			val firstUrn:Cite2Urn = obj.urn
			ObjectModel.objectOrCollection.value = "object"
			document.getElementById("object_urnInput").asInstanceOf[HTMLInputElement].value = firstUrn.toString
			ObjectView.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.doInsertFirstObjectUrn: ${e}")
		}
	}

	val queryGetObjects:String = "/objects/"

	def getObject(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
			// grab any DSE records that came with this corpus!
			val dseVec:Option[Vector[DseRecord]] = objJson.dsesForVectorOfCiteObjects(jstring)
			dseVec match {
				case Some(dv) => {
					DSEModel.updateDsesForCurrentObjects(dv)
				}
				case None => {
					DSEModel.clearDsesForCurrentObjects
				}
			}
			
			ObjectModel.collectionUrnCheck(urn.get.asInstanceOf[Cite2Urn])
			val vco:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
			vco.size match {
				case n if (n > 0) => {
					ObjectModel.boundObjects.value.clear	
				   vco.foreach( fc => {
						ObjectModel.boundObjects.value += fc
					})
					ObjectController.setDisplay
				}
				case _ => {
					ObjectModel.boundObjects.value.clear	
				}
			}

			// we'll do the same for Commentary eventually…	
			ObjectView.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.getBoundObjects: ${e}")
		}
	}

	val queryGetPaged:String = "/objects/paged/"

	def getRangeOrCollection(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
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
			// grab any DSE records that came with this corpus!
			val dseVec:Option[Vector[DseRecord]] = objJson.dsesForVectorOfCiteObjects(jstring)
			dseVec match {
				case Some(dv) => DSEModel.updateDsesForCurrentObjects(dv)
				case None => DSEModel.clearDsesForCurrentObjects
			}
			// we'll do the same for Commentary eventually…	
			ObjectModel.collectionUrnCheck(urn.get.asInstanceOf[Cite2Urn])
			vco.size match {
				case n if (n > 0) => {
					ObjectModel.boundObjects.value.clear	
				   vco.foreach( fc => {
						ObjectModel.boundObjects.value += fc
					})
					ObjectController.setDisplay
				}
				case _ => {
					ObjectModel.boundObjects.value.clear	
				}
			}
			ObjectView.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.getBoundObjects: ${e}")
		}
	}

	// prev/next

	val queryGetPrevUrn:String = "/objects/prevurn/"
	val queryGetNextUrn:String = "/objects/nexturn/"

	def getPrevUrn(jstring:String, urn:Option[Urn]):Unit = {
		try {
			val vco:Option[Cite2Urn] = objJson.cite2UrnString(jstring)
			ObjectModel.currentPrev.value = {
				vco match {
					case Some(u) => Some(Some(u), ObjectModel.offset.value, ObjectModel.limit.value)
					case None => None
				}
			}
			ObjectView.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.getPrevUrn: ${e}")
		}
	}
	def getNextUrn(jstring:String, urn:Option[Urn]):Unit = {
		try {
			val vco:Option[Cite2Urn] = objJson.cite2UrnString(jstring)
			ObjectModel.currentNext.value = {
				vco match {
					case Some(u) => Some(Some(u), ObjectModel.offset.value, ObjectModel.limit.value)
					case None => None
				}
			}
			ObjectView.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.getNextUrn: ${e}")
		}
	}

	// Collections URNS 
	val queryGetCollectionUrns:String = "/collections/reff/"

	def getCollectionUrns(jstring:String, urn:Option[Urn]):Unit = {
		try {
			val vco:Vector[Cite2Urn] = objJson.vectorOfCite2Urns(jstring)
			ObjectModel.boundCollectionUrns.value.clear
			for (u <- vco) {
				try {
					ObjectModel.boundCollectionUrns.value += u 
				} catch {
					case e:Exception => throw new Exception(s"Failed on ${u}.")
				}
			}
			val concreteUrn:Cite2Urn = urn.get.asInstanceOf[Cite2Urn]
			ObjectModel.setOffsetToCurrentUrn(urn.get.asInstanceOf[Cite2Urn])
		} catch {
			case e:Exception => throw new Exception(s"ObjectQuery.getNextUrn: ${e}")
		}
	}


}
