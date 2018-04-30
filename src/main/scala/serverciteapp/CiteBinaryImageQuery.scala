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

@JSExportTopLevel("serverciteapp.CiteBinaryImageQuery")
object CiteBinaryImageQuery {


	val objJson:CiteObjJson = CiteObjJson()
	val citeLibraryJson:CiteLibraryJson = CiteLibraryJson()

	/* A sequence that will build a CiteCollectionRepository of Image Collections */

	def initRepo1(jstring:String, urn:Option[Urn] = None):Unit = {
		// This should result in a vector of collectionsForModel
		val vcu:Vector[Cite2Urn] = objJson.vectorOfCite2Urns(jstring)
		vcu.size match {
			case s if (s > 0) => {
				CiteBinaryImageModel.hasBinaryImages.value = true
				for (c <- vcu) {
					CiteBinaryImageModel.binaryImageCollections.value += c
				}
				// Let's get a Catalog
				val queryString:String = s"/collections/"
				val task = Task{ CiteMainQuery.getJson(CiteBinaryImageQuery.initRepo2, queryString, urn = None) }
				val future = task.runAsync	
			}
			case _ =>  {
	  			CiteBinaryImageModel.hasBinaryImages.value = false
	  			CiteBinaryImageModel.hasLocalOption.value = false
	  			CiteBinaryImageModel.hasRemoteOption.value = false
			}
		}
	}

	def initRepo2(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
			val cat:CiteCatalog = objJson.citeCatalog(jstring)
			cat.size match {
				case n if (n > 0) => {
					// Let's filter that catalog down to just the ones we want	
					val tempVectorOfImageCollUrns:Vector[Cite2Urn] = CiteBinaryImageModel.binaryImageCollections.value.toVector
					val filteredCat:Vector[CiteCollectionDef] = {
						cat.collections.filter(c => {
							tempVectorOfImageCollUrns.contains(c.urn)
						})
					}
					val newCat:CiteCatalog = CiteCatalog(filteredCat)
					CiteBinaryImageModel.binaryImageCollectionCatalog.value = Some(newCat)
					// Now we need to get the data for this newly pared-down repo
					val collectionsInCatalog:Vector[Cite2Urn] = {
						newCat.collections.map(_.urn).toVector
					}
					val queryUrns:String = {
						collectionsInCatalog.mkString("&urn=")
					}	
					val queryString:String = s"/collections/objects?urn=${queryUrns}"
					val task = Task{ CiteMainQuery.getJson(CiteBinaryImageQuery.initRepo3, queryString, urn = None) }
					val future = task.runAsync	
					// and we hand it off to process all those objects…
				}
				case _ => {
					// No catalog… something is badly wrong	
		  			CiteBinaryImageModel.hasBinaryImages.value = false
		  			CiteBinaryImageModel.hasLocalOption.value = false
		  			CiteBinaryImageModel.hasRemoteOption.value = false
		  			throw new Exception("No catalog returned to CiteBinaryImageQuery.")
				}
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}


	def initRepo3(jstring:String, urn:Option[Urn] = None):Unit = {
		try {
			val citeObjects:Vector[CiteObject] = objJson.vectorOfCiteObjects(jstring)
			citeObjects.size match {
				case n if (n > 0) => {
					// Let's make this into a CiteObjectMap
					val citeObjectMap:CiteObjectMap = {
						CiteObjectMap(
				         Map(citeObjects.map{ a => a.urn -> a }: _*).toMap
				      )
			       }
			       // and stash it!
			     	 CiteBinaryImageModel.binaryImageCollectionObjects.value = Some(citeObjectMap) 
			     	 // And let's get a vector of propertyValues
			     	 val propVec:Vector[CitePropertyValue] = {
			     	 	citeObjects.map( obj => {
			     	 		obj.propertyList.map( prop => {
			     	 			CitePropertyValue(prop.urn, prop.propertyValue)
			     	 		}).toVector		
			     	 	}).toVector.flatten
			     	 }
			     	 // and stash it!
			     	 CiteBinaryImageModel.binaryImageCollectionData.value = Some(CiteCollectionData(propVec))
			     	 // And now we can create our own little custom CiteCollectionRepository for Image stuff!!!
			   	 CiteBinaryImageModel.binaryImageCollectionRepo.value = {
			   	 	Some(
				   	 	CiteCollectionRepository(
				   	 		CiteBinaryImageModel.binaryImageCollectionData.value.get,
				   	 		CiteBinaryImageModel.binaryImageCollectionObjects.value.get,
				   	 		CiteBinaryImageModel.binaryImageCollectionCatalog.value.get
				   	 	)
				   	 )
			   	 }  	 
			   	 CiteMainController.updateUserMessage("Binary Images are configured and ready.",0)
			   	 /* And finally… */
			   	 CiteBinaryImageController.discoverProtocols
				}
				case _ => {
					// No catalog… something is badly wrong	
		  			CiteBinaryImageModel.hasBinaryImages.value = false
		  			CiteBinaryImageModel.hasLocalOption.value = false
		  			CiteBinaryImageModel.hasRemoteOption.value = false
		  			throw new Exception("No objects returned to CiteBinaryImageQuery.")
				}
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}
	

	

}
