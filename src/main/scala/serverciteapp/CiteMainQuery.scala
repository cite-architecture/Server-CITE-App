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

@JSExportTopLevel("CiteMainQuery")
object CiteMainQuery {

	val citeLibraryJson:CiteLibraryJson = CiteLibraryJson()


	/*
		Use AJAX request to get remote data
		`callback` is the name of a function that should take a single parameter of type String
	*/
	def getJson(callback: (String, Option[Urn]) => Unit, query:String, url: String = CiteMainModel.serviceUrl.value, urn:Option[Urn] = None):Unit = {

		val xhr = new XMLHttpRequest()
		xhr.open("GET", s"${url}${query}" )
		xhr.onload = { (e: Event) =>
			if (xhr.status == 200) {
				val contents:String = xhr.responseText
				callback(contents, urn)
			} else {
				CiteMainController.updateUserMessage(s"Request for info on remote library failed with code ${xhr.status}",2)
			}
		}
		xhr.send()
}


	/* Queries */
	val queryLibraryInfo:String = "/libraryinfo"

	def updateLibraryMetadata(jstring:String, urn:Option[Urn] = None):Unit = {

		val libMap:scala.collection.immutable.Map[String,String] = citeLibraryJson.parseLibraryInfo(jstring)
		val libName:String = libMap("name")
		val libUrn:Cite2Urn = Cite2Urn(libMap("urn"))
		val libLicense:String = libMap("license")
		CiteMainModel.currentLibraryMetadataString.value = {
			s"Current Library: ${libName}. ${libUrn}. ${libLicense}. At: ${CiteMainModel.serviceUrl.value}"
		}	

	}

	/* DataModels */
	def getDataModels(jstring:String, urn:Option[Urn] =None):Unit ={
		val citeObjJson:CiteObjJson = CiteObjJson()
		val dms:Vector[DataModel] = citeObjJson.dataModels(jstring)
		val odms:Option[Vector[DataModel]] = {
			dms.size match {
				case n if (n > 1) => Some(dms)
				case _ => None
			}	
		}
		odms match {
			case Some(dm) => {
					DataModelModel.dataModels.value = Some(dm)
					CiteBinaryImageController.setImageSwitch
					/*
					CommentaryModel.loadAllComments
					*/
			
					// Start the process of building a binary image collection repo
					CiteBinaryImageModel.initBinaryImageRepo
			
				}
				case None => { 
					DataModelController.clearDataModels
				}
		}
	}	

}
