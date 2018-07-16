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
import edu.holycross.shot.dse._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citejson._
import edu.holycross.shot.citerelation._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("serverciteapp.O2Query")
object O2Query {


	val o2Json:Ohco2Json = Ohco2Json()
	val relationsJson:RelationsJson = RelationsJson()

	/* Queries */
	val queryCatalog:String = "/texts"

	/* This does a lot of work:
		- It sees if there is a TextRepository
		- If so, it lets O2Model know
		- It shows the Browse Texts and Explore Texts tabs
	*/
	def updateCatalog(jstring:String, urn:Option[Urn] = None):Unit = {
		val cat:Catalog = o2Json.o2Catalog(jstring)
		O2Model.currentCatalog.value = {
			cat.size match {
				case n if (n > 0) => Some(cat)
				case _ => None
			}
		} 
		O2Model.currentCatalog.value match {
			case Some(cat) => {
				O2Model.hasTextRepo.value = true
				O2Model.citedWorks.value.clear
				for ( cw <- cat.urnList){
					O2Model.citedWorks.value += cw
				}

				CiteMainModel.showTexts.value = true
				CiteMainModel.showNg.value = true

				O2Controller.updateUserMessage(s"Updated text repository with ${O2Model.citedWorks.value.size} versions of works.",0)
				CiteMainView.changeTab("text")

				O2Controller.preloadUrn

				// Load request parameter
				CiteMainModel.requestParameterUrn.value match {
					case Some(u) => {
						u match {
							case CtsUrn(ctsurn) => {
								DataModelController.retrieveTextPassage(None, CtsUrn(ctsurn))
								CiteMainView.changeTab("text")
							}
							case _ => // do nothing
						}
					}	
					case None => // do nothing
				}
			

			}
			case None => {
				O2Model.citedWorks.value.clear
				O2Model.hasTextRepo.value = false
				CiteMainModel.showTexts.value = false
			}
		}
	}

	// First Urn Stuff

	val queryFirstUrn:String = "/texts/firsturn/"

	def getFirstUrn(jstring:String, urn:Option[Urn] = None):Unit = {
		val ourn:Option[CtsUrn] = o2Json.o2CtsUrnString(jstring)
		ourn match {
			case Some(urn)	=> {
				O2Model.urn.value = urn
				O2Controller.validUrnInField.value = true
			}
			case None => {
				O2Model.urn.value = CtsUrn("urn:cts:ns:group.work.version.exemplar:passage")
				O2Controller.validUrnInField.value = false
			}
		}
	}

	def getFirstNodeUrn(jstring:String, urn:Option[Urn] = None):Unit = {
		val ourn:Option[CtsUrn] = o2Json.o2CtsUrnString(jstring)
		ourn match {
			case Some(urn) => {
				js.Dynamic.global.document.getElementById("o2_urnInput").value = urn.toString
				O2Controller.validUrnInField.value = true
			}
			case None => {
				js.Dynamic.global.document.getElementById("o2_urnInput").value = ""
				O2Controller.validUrnInField.value = false
			}

		}
	}

	// textCatalogStuff

	val queryTextCatalog:String = "/textcatalog"

	def getVersionsForUrn(jstring:String, urn:Option[Urn] = None):Unit = {
		val cat:Catalog = o2Json.o2Catalog(jstring)			
		O2Model.versionsForCurrentUrn.value = cat.size	
	}

	// label for urn
	val queryLabelForUrn:String = "/texts/label/"

	def getLabelForUrnHistory(s:String, urn:Option[Urn]):Unit = {
		try {
			val label:String = s
			urn match {
				case Some(u) =>  {
					u.getClass.getName match {
						case "edu.holycross.shot.cite.CtsUrn" => O2Model.updateUrnHistory(u.asInstanceOf[CtsUrn], label)
						case _ => throw new Exception(s"${u} is of class ${u.getClass.getName}")
					}
				}
				case None => throw new Exception(s"Did not get URN along with label: ${label}.")
			}	
		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}


	// corpus!!

	val queryGetCorpus:String = "/texts/"

	def getCorpus(s:String, urn:Option[Urn]):Unit = {
		try {
			val newUrn = {
				urn match {
					case Some(u) => {
						u.getClass.getName match {
							case "edu.holycross.shot.cite.CtsUrn" => u.asInstanceOf[CtsUrn]
							case _ => throw new Exception(s"${u} is not a CtsUrn: ${u.getClass.getName}.")
						}	
					}
					case None => throw new Exception("No urn given for O2Query.getCorpus.")
				}
			}
			val vcn:Vector[CitableNode] = o2Json.o2VectorOfCitableNodes(s)

			// grab any DSE records that came with this corpus!
			val dseVec:Option[Vector[DseRecord]] = o2Json.dsesForCorpus(s)
			dseVec match {
				case Some(dv) => {
					DSEModel.updateDsesForCurrentText(dv)
				}
				case None => DSEModel.clearDsesForCurrentText
			}
			// we'll do the same for Commentary eventually…	
			val commentsVec:Option[Vector[CiteTriple]] = o2Json.commentaryForCorpus(s)
			commentsVec match {
				case Some(cv) => {
					CommentaryModel.updateCurrentListOfComments(cv)
				}
				case None => CommentaryModel.clearComments
			}


			val tempCorpus:Corpus = Corpus(vcn)
			O2Model.updateCurrentListOfUrns(tempCorpus)
			//DSEModel.updateCurrentListOfDseUrns(tempCorpus)
			//CommentaryModel.updateCurrentListOfComments(tempCorpus)
		   O2Model.updateCurrentCorpus(tempCorpus, newUrn)
			O2Model.currentNumberOfCitableNodes.value = tempCorpus.size
			O2View.cursorNormal
		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}

	// prev/next
	val queryGetPrev:String = "/texts/prevurn/"
	val queryGetNext:String = "/texts/nexturn/"

	def getPrev(jstring:String, urn:Option[Urn]):Unit = {
		try {
			val ourn:Option[CtsUrn] = o2Json.o2CtsUrnString(jstring)
			ourn match {
				case Some(urn) => O2Model.currentPrev.value = Some(urn)
				case None =>
			}

		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}
	def getNext(jstring:String, urn:Option[Urn]):Unit = {
		try {
			val ourn:Option[CtsUrn] = o2Json.o2CtsUrnString(jstring)
			ourn match {
				case Some(urn) => O2Model.currentNext.value = Some(urn)
				case None =>
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")
		}
	}





}
