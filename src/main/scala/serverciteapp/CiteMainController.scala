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

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.CiteMainController")
object CiteMainController {


	/* 
		Initiate app with a URL to an online CEX file	
	*/
	@JSExport
	def main(serviceUrl: String): Unit = {
		CiteMainModel.serviceUrl.value = serviceUrl
		CiteMainController.updateUserMessage("Getting library information…",1)
		CiteMainQuery.getJson(CiteMainQuery.updateLibraryMetadata, CiteMainQuery.queryLibraryInfo, urn = None)
		updateRepository

		dom.render(document.body, CiteMainView.mainDiv)
	}

	/*
	 	Handles displaying messages to the user, color-coded according to type.
	 	Fades after 10 seconds.		
	*/
	def updateUserMessage(msg: String, alert: Int): Unit = {
		CiteMainModel.userMessageVisibility.value = "app_visible"
		CiteMainModel.userMessage.value = msg
		alert match {
			case 0 => CiteMainModel.userAlert.value = "default"
			case 1 => CiteMainModel.userAlert.value = "wait"
			case 2 => CiteMainModel.userAlert.value = "warn"
		}
		js.timers.clearTimeout(CiteMainModel.msgTimer)
		CiteMainModel.msgTimer = js.timers.setTimeout(10000){ CiteMainModel.userMessageVisibility.value = "app_hidden" }
	}


	/*
		Hide all tabs. Done initially. Tabs are shown based on the contexts
		of the CEX library.
	*/
	def hideTabs:Unit = {
	  CiteMainModel.showTexts.value = false 
	  CiteMainModel.showNg.value = false
	  CiteMainModel.showCollections.value = false
	  CiteMainModel.showImages.value = false
	  CiteMainModel.showRelations.value = false
	}


	/*
			Clear all data.
	*/
	def clearRepositories:Unit = {
		O2Model.currentCatalog.value = None
		NGController.clearResults
		NGModel.corpusOrUrn.value = None
		ObjectModel.collections.value.clear
		ObjectModel.labelMap.value = None
		CiteBinaryImageModel.binaryImageCollectionRepo.value = None
		/*
		CiteMainModel.mainLibrary.value = None
		CommentaryModel.clearComments
		*/
	}


	// Sets up repository
	@dom
	def updateRepository = {

		hideTabs
		clearRepositories

		

		try {

			/* Text Repository Stuff
			-------------------------------------
			*/
			O2Model.updateCitedWorks // which hands off to O2Query.updateCatalog
			// O2Query.updateCatalog, in turn, takes care of activating the "Explore Texts" tab.

			// Collection Repository Stuff
			ObjectModel.updateCollections // which hands off to ObjectQuery.updateCatalog
			// ObjectQuery.updateCatalog, in turn, takes care of activating the "Collections" tab.



			// Relations stuff
			RelationsModel.updateRelations
			// Data Model Stuff
			//     the datamodel task will, in turn, start the process
			//     of building out CiteBinaryImages
			val dmTask = Task{ CiteMainQuery.getJson(CiteMainQuery.getDataModels, s"/datamodels", urn = None) }
			val dmFuture = dmTask.runAsync	

			// Load request parameter
			CiteMainModel.requestParameterUrn.value match {
				case Some(u) => {
					u match {
						case CtsUrn(ctsurn) => {
							//DataModelController.retrieveTextPassage(None, CtsUrn(ctsurn))
						}
						case Cite2Urn(cite2urn) => {
							//DataModelController.retrieveObject(None, Cite2Urn(cite2urn))
						}
						case _ => // do nothing
					}
				}	
				case None => // do nothing
			}

		} catch  {
			case e: Exception => {
				CiteMainController.updateUserMessage(s"""${e}. Invalid CEX file.""",2)
			}
		}

	}

	def getRequestUrn:Option[Urn] = {
	val currentUrl = 	js.Dynamic.global.location.href
		val requestParamUrnString = currentUrl.toString.split('?')
		val requestUrn:Option[Urn] = requestParamUrnString.size match {
			case s if (s > 1) => {
				try {
					val parts = requestParamUrnString(1).split("=")
					if ( parts.size > 1) {
						if ( parts(0) == "urn" ) {
							val decryptedString:String = js.URIUtils.decodeURIComponent(parts(1))
							val decryptedUrn:Option[Urn] = {
								parts(1).take(8) match {
									case ("urn:cts:") => Some(CtsUrn(decryptedString))
									case ("urn:cite") => Some(Cite2Urn(decryptedString))
									case _ => {
										None
									}
								}
							}
							decryptedUrn
						} else {
							None
						}
					} else {
						None
					}
				} catch {
					case e:Exception => {
						CiteMainController.updateUserMessage(s"Failed to load request-parameter URN: ${e}",1)
						None
					}
				}
			}
			case _  => {
				None
			}
		}
		requestUrn
	}


}
