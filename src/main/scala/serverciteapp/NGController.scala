package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import scala.scalajs.js.Dynamic.{ global => g }
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("serverciteapp.NGController")
object NGController {


	def constructStringSearchObject:NGModel.StringSearch = {
		val s: String = js.Dynamic.global.document.getElementById("stringSearch_Input").value.toString
		val ssq = NGModel.StringSearch(s, NGModel.corpusOrUrn.value)
		ssq
	}

	def constructTokenSearchObject:NGModel.TokenSearch = {
		val s: String = js.Dynamic.global.document.getElementById("tokenSearch_Input").value.toString
		val prox = NGModel.tokenSearchProximity.value
		val searchVector:Vector[String] = s.split(" ").toVector
		val tsq = NGModel.TokenSearch(searchVector, prox, NGModel.corpusOrUrn.value)
		tsq
	}


	def constructNGramQueryObject:NGModel.NGramQuery = {
		val n:Int = js.Dynamic.global.document.getElementById("ngram_nlist").value.toString.toInt
		val occ:Int = js.Dynamic.global.document.getElementById("ngram_minOccurrances").value.toString.toInt
		val ignorePuncString: String = js.Dynamic.global.document.getElementById("ngram_ignorePuncBox").checked.toString
		val ignorePunc: Boolean = (ignorePuncString == "true")
		val filterString: String = js.Dynamic.global.document.getElementById("ngram_filterStringField").value.toString

		val ngq = NGModel.NGramQuery(n, occ, filterString, ignorePunc, NGModel.corpusOrUrn.value )
		ngq
	}

	def loadQuery(q:NGModel.StringSearch) = {
		NGController.clearInputs
		NGController.clearResults
		js.Dynamic.global.document.getElementById("stringSearch_Input").value = q.fs
		NGModel.corpusOrUrn.value = q.urn
	}

	def loadQuery(q:NGModel.TokenSearch) = {
		NGController.clearInputs
		NGController.clearResults
		js.Dynamic.global.document.getElementById("tokenSearch_Input").value = q.tt.mkString(" ")
		NGModel.tokenSearchProximity.value = q.p
		NGModel.corpusOrUrn.value = q.urn
	}

	def loadQuery(q:NGModel.NGramQuery) = {
		NGController.clearInputs
		NGController.clearResults
		js.Dynamic.global.document.getElementById("ngram_nlist").value = q.n.toString
		js.Dynamic.global.document.getElementById("ngram_minOccurrances").value = q.t.toString
		js.Dynamic.global.document.getElementById("ngram_ignorePuncBox").checked = q.ip.toString
		js.Dynamic.global.document.getElementById("ngram_filterStringField").value = q.fs
		NGModel.corpusOrUrn.value = q.urn
	}

	def executeQuery(q:NGModel.StringSearch):Unit = {
		NGView.cursorWaiting
		NGController.clearResults
		NGController.updateUserMessage(s"""Searching for string "${q.fs}". Please be patient…""",1)
		NGModel.nGramResults.value.clear
		NGModel.citationResults.value.clear
		q.urn match {
			case Some(u) => {
				val task = Task{ CiteMainQuery.getJson(NGQuery.getFindString, s"${NGQuery.queryFindString}/${u}?s=${q.fs}", urn = q.urn) }
				val future = task.runAsync	
			}
			case None => {
				val task = Task{ CiteMainQuery.getJson(NGQuery.getFindString, s"${NGQuery.queryFindString}?s=${q.fs}", urn = q.urn) }
				val future = task.runAsync	
			}
		}
	}

	def executeQuery(q:NGModel.NGramQuery):Unit = {
		NGView.cursorWaiting
		NGController.clearResults
		NGController.updateUserMessage("Getting N-Grams. Please be patient…",0)
//		NGModel.nGramResults.value.clear
//		NGModel.citationResults.value.clear
		val qString:String = {
			q.urn match {
				case Some(u) => {
					val tempS:String = s"${NGQuery.queryFindNgrams}/${u}" +
						s"?n=${q.n}" + 
						s"&t=${q.t}" + 
						{
							q.fs match {
								case "" => ""
								case _ => s"&s=${q.fs}"
							}
						}
					tempS
				}
				case None => {
					val tempS:String = s"${NGQuery.queryFindNgrams}" +
						s"?n=${q.n}" + 
						s"&t=${q.t}" + 
						{
							q.fs match {
								case "" => ""
								case _ => s"&s=${q.fs}"
							}
						}
						tempS
				}
			}	
		}	
		//g.console.log(s"Querying for: ${qString}")
		
		val task = Task{ CiteMainQuery.getJson(NGQuery.getFindNgrams, qString, urn = q.urn) }
		val future = task.runAsync	
	}


def executeQuery(q:NGModel.TokenSearch):Unit = {
	NGView.cursorWaiting
	NGController.clearResults
	NGController.updateUserMessage(s"""Searching for tokens "${q.tt.mkString(" ")}". Please be patient…""",1)
	//NGModel.nGramResults.value.clear
	//NGModel.citationResults.value.clear

		val qString:String = {
			val tt:String = {
				s"""&t=${q.tt.mkString("&t=")}"""		
			}
			q.urn match {
				case Some(u) => {
					val tempS:String = s"${NGQuery.queryTokenSearch}/${u}" +
						s"?dist=${q.p}" + 
						s"&tt=${tt}"
					tempS
				}
				case None => {
					val tempS:String = s"${NGQuery.queryTokenSearch}" +
						s"?dist=${q.p}" + 
						s"&tt=${tt}"
					tempS
				}
			}	
		}	
		
		val task = Task{ CiteMainQuery.getJson(NGQuery.getTokenSearch, qString, urn = q.urn) }
		val future = task.runAsync	
	}




	def executeQuery(q:NGModel.CtsQuery):Unit = {
		q.getClass.getName match {
			case "citeapp.NGModel$NGramQuery" => {
				NGController.executeQuery(q.asInstanceOf[NGModel.NGramQuery])
			}
			case "citeapp.NGModel$StringSearch" => {
				NGController.executeQuery(q.asInstanceOf[NGModel.StringSearch])
			}
			case "citeapp.NGModel$TokenSearch" => {
				NGController.executeQuery(q.asInstanceOf[NGModel.TokenSearch])
			}
			case _ => NGController.updateUserMessage("Unrecognized type of search!",2)
		}
	}

	def loadQuery(q:NGModel.CtsQuery):Unit = {
		q.getClass.getName match {
			case "citeapp.NGModel$NGramQuery" => {
				NGController.loadQuery(q.asInstanceOf[NGModel.NGramQuery])
			}
			case "citeapp.NGModel$StringSearch" => {
				NGController.loadQuery(q.asInstanceOf[NGModel.StringSearch])
			}
			case "citeapp.NGModel$TokenSearch" => {
				NGController.loadQuery(q.asInstanceOf[NGModel.TokenSearch])
			}
			case _ => NGController.updateUserMessage("Unrecognized type of search!",2)
		}
	}


	def nGramQuery:Unit = {
		val newQuery = NGController.constructNGramQueryObject

		if (O2Model.hasTextRepo.value == false){
			NGController.updateUserMessage(s"No library loaded. O2Model.hasTextRepo == ${O2Model.hasTextRepo.value}",2)
		} else {
			NGModel.pastQueries.value += newQuery
			NGController.executeQuery(newQuery)
		}

	}

	def tokenSearchQuery:Unit = {
		val newQuery = NGController.constructTokenSearchObject

		if (O2Model.hasTextRepo.value == false){
			NGController.updateUserMessage(s"No library loaded. O2Model.hasTextRepo == ${O2Model.hasTextRepo.value}",2)
		} else {
			NGModel.pastQueries.value += newQuery
			NGController.executeQuery(newQuery)
		}
	}

	/* Sequence is:
		1. NGController.stringSearchQuery
		2. NGCotrolller.executeQuery
		3. NGQuery.getFindString
	*/
	def stringSearchQuery:Unit = {
		// Here we construct the String Search Object
		val newQuery = NGController.constructStringSearchObject

		if (O2Model.hasTextRepo.value == false){
			NGController.updateUserMessage(s"No library loaded. O2Model.hasTextRepo == ${O2Model.hasTextRepo.value}",2)
		} else {
			NGModel.pastQueries.value += newQuery
			NGController.executeQuery(newQuery)
		}
	}


	def clearResults: Unit = {
		NGModel.nGramResults.value.clear
		NGModel.citationResults.value.clear
		NGModel.nGramQueryReport.value = ""
		NGModel.otherQueryReport.value = ""
		NGModel.currentStringSearchResults.value = None	
	}

	def clearInputs: Unit = {
		js.Dynamic.global.document.getElementById("ngram_filterStringField").value = ""
		js.Dynamic.global.document.getElementById("stringSearch_Input").value = ""
		js.Dynamic.global.document.getElementById("tokenSearch_Input").value = ""
	}

	def clearHistory:Unit = {
		NGModel.pastQueries.value.clear
	}

	def getUrnsForNGram(s: String): Unit = {
		NGView.cursorWaiting
		val occ:Int = js.Dynamic.global.document.getElementById("ngram_minOccurrances").value.toString.toInt
		val ignorePuncString: String = js.Dynamic.global.document.getElementById("ngram_ignorePuncBox").checked.toString
		val ignorePunc: Boolean = (ignorePuncString == "true")
		NGController.updateUserMessage("Getting URNs for N-Grams. Please be patient…",0)

		if (O2Model.hasTextRepo.value == false){
			NGController.updateUserMessage("No library loaded.",2)
		} else {
			NGModel.citationResults.value.clear
			NGModel.corpusOrUrn.value match {
					case Some(urn:CtsUrn) => {
						val qString:String = {
							NGQuery.queryCorpusForNgrams + 
							s"/${urn}" + s"?ng=${s}"
						} 
						val task = Task{ CiteMainQuery.getJson(NGQuery.getCorpusForNgram, qString, urn = Some(urn)) }
						val future = task.runAsync	
					}
					case None => {
						val qString:String = {
							NGQuery.queryCorpusForNgrams + 
							s"?ng=${s}"
						} 
						val task = Task{ CiteMainQuery.getJson(NGQuery.getCorpusForNgram, qString, urn = None) }
						val future = task.runAsync	
					}
			}
		}
	}
	
	def updateUserMessage(msg: String, alert: Int): Unit = {
		NGModel.userMessageVisibility.value = "app_visible"
		NGModel.userMessage.value = msg
		alert match {
			case 0 => NGModel.userAlert.value = "default"
			case 1 => NGModel.userAlert.value = "wait"
			case 2 => NGModel.userAlert.value = "warn"
		}
		js.timers.clearTimeout(NGModel.msgTimer)
		NGModel.msgTimer = js.timers.setTimeout(6000){ NGModel.userMessageVisibility.value = "app_hidden" }
	}

/*
	@dom
	def preloadUrn = {
		NGModel.urn.value = O2Model.textRepo.value.get.corpus.firstNode(O2Model.textRepo.value.get.corpus.citedWorks(0)).urn
		NGModel.updateShortWorkLabel
	}
	*/

	def validateThresholdEntry(thisEvent: Event):Unit = {
		val thisTarget = thisEvent.target.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
		val testText = thisTarget.value.toString
		try{
			val mo: Int = testText.toInt
			NGModel.nGramThreshold.value = mo
		} catch {
			case e: Exception => {
				val badMo: String = testText
				NGModel.nGramThreshold.value = 3
				NGController.updateUserMessage(s"Minimum Occurrances value must be an integer. '${badMo}' is not an integer.", 2)
				js.Dynamic.global.document.getElementById("ngram_minOccurrances").value =  NGModel.nGramThreshold.value.toString
			}
		}
	}
	def validateProximityEntry(thisEvent: Event):Unit = {
		val thisTarget = thisEvent.target.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
		val testText = thisTarget.value.toString
		try{
			val mo: Int = testText.toInt
			NGModel.tokenSearchProximity.value = mo
		} catch {
			case e: Exception => {
				val badMo: String = testText
				NGModel.tokenSearchProximity.value = 3
				NGController.updateUserMessage(s"Proximity value must be an integer. '${badMo}' is not an integer.", 2)
				js.Dynamic.global.document.getElementById("tokenSearch_proximityInput").value =  NGModel.tokenSearchProximity.value.toString
			}
		}
	}

}
