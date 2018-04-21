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

@JSExportTopLevel("serverciteapp.NGQuery")
object NGQuery {


	val o2Json:Ohco2Json = Ohco2Json()

	/* Queries */
	val queryCatalog:String = "/texts"

	def updateCatalog(jstring:String, urn:Option[Urn] = None):Unit = {
		val cat:Catalog = o2Json.o2Catalog(jstring)
		NGModel.currentCatalog.value = {
			cat.size match {
				case n if (n > 0) => Some(cat)
				case _ => None
			}
		} 
		NGModel.currentCatalog.value match {
			case Some(cat) => {
				NGModel.hasTextRepo.value = true
				NGModel.citedWorks.value.clear
				for ( cw <- cat.urnList){
					NGModel.citedWorks.value += cw
				}
				CiteMainModel.showNg.value = true
				//NGController.updateUserMessage(s"Updated text repository with ${O2Model.citedWorks.value.size} versions of works.",0)
			}
			case None => {
				NGModel.citedWorks.value.clear
				NGModel.hasTextRepo.value = false
				CiteMainModel.showNg.value = false
			}
		}
	}



	// label for urn
	val queryLabelForUrn:String = "/texts/label/"

	def getLabelForUrnHistory(s:String, urn:Option[CtsUrn]):Unit = {
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

	// find String

	val queryFindString:String = "/texts/find"

	def getFindString(s:String, urn:Option[Urn] = None):Unit = {
		try {
			val u:Option[CtsUrn] = {
				urn match {
					case Some(u) => {
						u.getClass.getName match {
							case "edu.holycross.shot.cite.CtsUrn" => Some(u.asInstanceOf[CtsUrn])
							case _ => throw new Exception(s"Could not make ${u} ( ${u.getClass.getName} ) into a CtsUrn.")
						}
					}
					case None => None
				}
			}
			val fs:String = NGModel.pastQueries.value.last.asInstanceOf[NGModel.StringSearch].fs
			val q:NGModel.StringSearch = NGModel.StringSearch(fs, u)
			val vcn:Vector[CitableNode] = o2Json.o2VectorOfCitableNodes(s)
			vcn.size match {
				case n if (n > 0) => {
					NGModel.currentStringSearchResults.value = Some(Corpus(vcn))
					NGModel.currentStringSearchResults.value match {
						case Some(results) => {
							for (n <- results.nodes){
									NGModel.citationResults.value += NGModel.SearchResult(Var(n.urn), Var(n.kwic(q.fs,20)))
							}
						}
						case None => // do nothing

					}
					NGModel.otherQueryReport.value = s"""Query: ${NGModel.pastQueries.value.last.asInstanceOf[NGModel.StringSearch].toString} Results: ${NGModel.citationResults.value.size}."""
					NGController.updateUserMessage(s"Found ${NGModel.citationResults.value.size} passages.",0)
				}
				case _ => NGModel.currentStringSearchResults.value = None
			}
		} catch {
			case e:Exception => {
				NGController.updateUserMessage(s"Exception in NGQuery.getFindstring: ${e}", 2)
				throw new Exception(s"NGQuery.getFindstring: ${e}")
			}
		}
	}





}
