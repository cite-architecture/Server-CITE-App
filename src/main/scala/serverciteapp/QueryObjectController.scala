package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import collection.mutable
import collection.mutable._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.scalajs.js.Dynamic.{ global => g }
import scala.scalajs.js.annotation.JSExport
import js.annotation._
import monix.execution.Scheduler.Implicits.global
import monix.eval._


@JSExportTopLevel("serverciteapp.QueryObjectController")
object QueryObjectController {

	def isValidSearch:Unit = {
		//We need at least a propertyType
		// one num, or two+range || string || urn || boolean || vocab
		var isValid = false

		QueryObjectModel.selectedPropertyType.value match {
			case Some(StringType) =>{
				if (QueryObjectModel.currentSearchString.value != None ){ isValid = true }
			}
			case Some(BooleanType) =>{
				 isValid = true
			}
			case Some(NumericType) =>{
				if (QueryObjectModel.currentNumericQuery1.value != None ){
					QueryObjectModel.currentNumericOperator.value match {
						case "inRange" => {
							if (QueryObjectModel.currentNumericQuery2.value != None ){ isValid = true }
						}
						case _ => isValid = true
					}
				}
			}
			case Some(ControlledVocabType) =>{
				if (QueryObjectModel.currentControlledVocabItem.value != None ){ isValid = true }
			}
			case Some(CtsUrnType) =>{
				if (QueryObjectModel.currentCtsUrnQuery.value != None ){ isValid = true }
			}
			case Some(Cite2UrnType) =>{
				if (QueryObjectModel.currentCite2UrnQuery.value != None ){ isValid = true }
			}
			case _ => { isValid = false }
		}
		QueryObjectModel.isValidSearch.value =  isValid

	}


	def initQuery:Unit = {
		// Clear URN field
		js.Dynamic.global.document.getElementById("object_urnInput").value = ""
		try {
			QueryObjectModel.selectedPropertyType.value match {
				case Some(StringType) => initStringSearch
				case Some(NumericType) => initNumericSearch
				case Some(ControlledVocabType) => initContVocabSearch
				case Some(BooleanType) => initBooleanSearch
				case Some(CtsUrnType) => initCtsUrnSearch
				case Some(Cite2UrnType) => initCite2UrnSearch
				case _ => { throw new Exception("unrecognized type")}
			}

		} catch {
			case e: Exception => {
				ObjectController.updateUserMessage(s"Cannot construct query-object from the given parameters. ${e}",2)
			}
		}
	}

	def loadQuery(q:QueryObjectModel.CiteCollectionQuery):Unit = {
		QueryObjectModel.currentQuery.value =  Some(q)
		QueryObjectModel.selectedPropertyType.value =  q.qPropertyType
		QueryObjectModel.currentQueryCollection.value = q.qCollection
		// -----
		QueryObjectModel.queryProperty.value = q.qProperty
		QueryObjectModel.currentControlledVocabItem.value = q.qControlledVocabItem
		QueryObjectModel.currentSearchString.value = q.qSearchString
		QueryObjectModel.currentRegexState.value = { 
			q.qRegex match {
				case Some(b) => b
				case None => false
			}
		}
		QueryObjectModel.currentCaseSensitiveState.value = {
			q.qCaseSensitive match {
				case Some(b) => b
				case None => false
			}
		}
		QueryObjectModel.currentNumericQuery1.value = q.qNum1
		QueryObjectModel.currentNumericQuery2.value = q.qNum2
		QueryObjectModel.currentNumericOperator.value = {
			q.qNumOperator match {
				case Some(s) => s	
				case None => ""
			}
		}
		QueryObjectModel.currentBooleanVal.value = {
			q.qBoolVal match {
				case Some(b) => b
				case None => false
			}
		}
		QueryObjectModel.currentCtsUrnQuery.value = q.qCtsUrn
		QueryObjectModel.currentCite2UrnQuery.value = q.qCite2Urn
		// ------
		initQuery
	}


	def initStringSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qSearchString = QueryObjectModel.currentSearchString.value,
			qCaseSensitive = Some(QueryObjectModel.currentCaseSensitiveState.value),
			qRegex = Some(QueryObjectModel.currentRegexState.value)
		)
		doStringSearch(cq)
	}

	def doStringSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
		val queryString:String = {
			if (cq.qRegex.get) {
				s"/objects/find/regexmatch${collString}?find=${cq.qSearchString.get}${propString}"
			} else {
				s"/objects/find/stringcontains${collString}?find=${cq.qSearchString.get}${propString}"
			}
		}
		//g.console.log(queryString)
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}

	def initNumericSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qNum1 = QueryObjectModel.currentNumericQuery1.value,
			qNum2 = QueryObjectModel.currentNumericQuery2.value,
			qNumOperator = Some(QueryObjectModel.currentNumericOperator.value)
		)
		doNumericSearch(cq)
	}

	def doNumericSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit  = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
	   val queryString:String = {
			cq.qNumOperator.get match {
				case "eq" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&op=${cq.qNumOperator.get}${propString}"
				}
				case "lt" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&op=${cq.qNumOperator.get}${propString}"
				}
				case "gt" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&op=${cq.qNumOperator.get}${propString}"
				}
				case "lteq" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&op=${cq.qNumOperator.get}${propString}"
				}
				case "gteq" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&op=${cq.qNumOperator.get}${propString}"
				}
				case "inRange" => {
					s"/objects/find/numeric${collString}?n1=${cq.qNum1.get}&n2=${cq.qNum2.get}&op=within${propString}"
				}
				case _ @ op => {
					ObjectController.updateUserMessage(s"Unrecognized numeric operator: ${op}.",2)
					throw new Exception(s"${op} is not a recognized numeric operator.")
				}
			}
		}	
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}


	def initContVocabSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qControlledVocabItem = QueryObjectModel.currentControlledVocabItem.value
		)
		doContVocabSearch(cq)
	}

	def doContVocabSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
	   val queryString:String = {
	   	s"/objects/find/regexmatch${collString}?find=${cq.qControlledVocabItem.get}${propString}"	
		}
		//g.console.log(queryString)
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}

	def initBooleanSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qBoolVal = Some(QueryObjectModel.currentBooleanVal.value)
		)
		doBooleanSearch(cq)
	}

	def doBooleanSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
	   val queryString:String = {
	   	s"/objects/find/valueequals${collString}?value=${cq.qBoolVal.get}&type=boolean${propString}"	
	   }
		//g.console.log(queryString)
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}

	def initCtsUrnSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qCtsUrn = QueryObjectModel.currentCtsUrnQuery.value
		)
		doCtsUrnSearch(cq)
	}

	def doCtsUrnSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
	   val queryString:String = {
	   	s"/objects/find/urnmatch${collString}?find=${cq.qCtsUrn.get}${propString}"
	   }	
		//g.console.log(queryString)
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}

	def initCite2UrnSearch:Unit = {
		val collUrn = {
			QueryObjectModel.currentQueryCollection.value match {
				case None => None
				case Some(u) => Some(u)
			}
		}
		val cq = QueryObjectModel.CiteCollectionQuery(
			qCollection = collUrn,
			qProperty = QueryObjectModel.queryProperty.value,
			qPropertyType = QueryObjectModel.selectedPropertyType.value,
			qCite2Urn =  QueryObjectModel.currentCite2UrnQuery.value
		)
		doCite2UrnSearch(cq)
	}

	def doCite2UrnSearch(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
		ObjectView.cursorWaiting
	   QueryObjectModel.currentQuery.value =  Some(cq)
	   val propString:String = {
	   	cq.qProperty match {
	   		case Some(p) => s"&propertyurn=${p}"
	   		case None => ""
	   	}
	   }
	   val collString:String = {
	   	cq.qCollection match {
	   		case Some(c) => s"/${c}"
	   		case None => ""
	   	}
	   }
	   val queryString:String = {
	   	s"/objects/find/urnmatch${collString}?find=${cq.qCite2Urn.get}${propString}"
	   }	
		//g.console.log(queryString)
		val task = Task{ CiteMainQuery.getJson(QueryObjectQuery.doQuery, queryString, urn = None) }
		val future = task.runAsync	
	}

	def loadSearchResults(ov:Vector[CiteObject]):Unit = {
				//cq.numResults = ov.size
				if (ov.size > 0 ){
						ov.size match {
							case 1 => ObjectController.updateUserMessage(s"Search found ${ov.size} matching object.",0)
							case _ => ObjectController.updateUserMessage(s"Search found ${ov.size} matching objects.",0)
						}
						ObjectModel.clearObject
						//QueryObjectModel.currentQuery.value =  Some(cq)
						addToSearchHistory(QueryObjectModel.currentQuery.value.get)
						ObjectModel.objectOrCollection.value =  "search"
						if (ObjectModel.limit.value > ov.size){ ObjectModel.limit.value =  ov.size }
						ObjectModel.offset.value =  1
						ObjectModel.browsable.value =  true
					  // display objects
						ObjectModel.clearObject
						ObjectModel.offset.value =  1
						if (ObjectModel.limit.value > ov.size){
							ObjectModel.limit.value =  ov.size
						}
						ObjectModel.browsable.value =  true
						ObjectModel.objectOrCollection.value =  "search"
						for (o <- ov ){
							ObjectModel.boundObjects.value += o
						}
						ObjectController.setDisplay
				} else {
						ObjectModel.clearObject
						//QueryObjectModel.currentQuery.value =  Some(cq)
						ObjectController.updateUserMessage("Search found no matching objects.",1)
				}

	}

	def addToSearchHistory(cq:QueryObjectModel.CiteCollectionQuery):Unit = {
			var vList = new ListBuffer[QueryObjectModel.CiteCollectionQuery]
			for (qh <- QueryObjectModel.pastQueries.value ){
				vList += qh
			}
			vList += cq
			val vSet = vList.toSet
			QueryObjectModel.pastQueries.value.clear
			for (q <- vSet ){ QueryObjectModel.pastQueries.value += q }

	}

}
