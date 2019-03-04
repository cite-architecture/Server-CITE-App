package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.Dynamic.{ global => g }
import scala.collection.mutable.LinkedHashMap
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.collection.immutable.SortedMap
import scala.concurrent._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("O2Model")
object O2Model {

	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

	case class VersionNodeBlock(versionUrn:Var[CtsUrn],nodes:Vars[CitableNode])

	case class BoundCorpus(versionUrn:Var[CtsUrn], versionLabel:Var[String], versionNodes:Vars[VersionNodeBlock] )

	val currentCorpus = Vars.empty[BoundCorpus]

	val currentNumberOfCitableNodes = Var(0)
	val currentListOfUrns = Vars.empty[CtsUrn]
	val isRtlPassage = Var(false)

	// for navigation
	val urnHistory = Vars.empty[(Int,CtsUrn,String)]


	// Add an entry to the citation-history
	def updateUrnHistory(u:CtsUrn, label:String):Unit = {
		try {
			if (hasTextRepo.value) {
				val tempList:List[Tuple2[CtsUrn,String]] = urnHistory.value.toList.reverse.map(t => { Tuple2(t._2, t._3)})
				val newTempList:List[Tuple2[CtsUrn,String]] = tempList ++ List(Tuple2(u,label))
				val indexedTempList:List[Tuple3[Int,CtsUrn,String]] = newTempList.zipWithIndex.map( t => {
					Tuple3((t._2 + 1),t._1._1,t._1._2)
				})
				val reversedList = indexedTempList.reverse
				urnHistory.value.clear
				for (t <- reversedList) { urnHistory.value += t }
			}
		} catch{
			case e:Exception => O2Controller.updateUserMessage(s"Unable to make label for ${u}: ${e}",2)
		}
	}


	// urn is what the user requested
	val urn = Var(CtsUrn("urn:cts:ns:group.work.version.exemplar:passage"))
	// displayUrn is what will be shown
	val displayUrn = Var(CtsUrn("urn:cts:ns:group.work.version.exemplar:passage"))
	val versionsForCurrentUrn = Var(1)

	val userMessage = Var("")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")

	val hasTextRepo = Var(false)
	val citedWorks = Vars.empty[CtsUrn]
	val currentCatalog = Var[Option[Catalog]](None)

	val currentNext = Var[Option[CtsUrn]](None)
	val currentPrev = Var[Option[CtsUrn]](None)


	/* Some methods for working the model */
	def versionsForUrn(urn:CtsUrn):Int = {
		var versions = 0
		if (O2Model.hasTextRepo.value){
				val s = s"urn:cts:${urn.namespace}:${urn.textGroup}.${urn.work}:"
				val u = CtsUrn(s)
				val task = Task{ CiteMainQuery.getJson(O2Query.getVersionsForUrn, s"${O2Query.queryTextCatalog}/${u}", urn = None) }
				val future = task.runAsync
		}
		versions
	}

	def getPrevNextUrn(urn:CtsUrn):Unit = {
				O2Model.currentPrev.value = None
				O2Model.currentNext.value = None
				val taskPrev = Task{ CiteMainQuery.getJson(O2Query.getPrev, s"${O2Query.queryGetPrev}${urn}", urn = None) }
				val futurePrev = taskPrev.runAsync
				val taskNext = Task{ CiteMainQuery.getJson(O2Query.getNext, s"${O2Query.queryGetNext}${urn}", urn = None) }
				val futureNext = taskNext.runAsync
	}

	def collapseToWorkUrn(urn:CtsUrn):CtsUrn = {
		val s = {
			s"urn:cts:${urn.namespace}:${urn.textGroup}.${urn.work}:${urn.passageComponent}"
		}
		val u = CtsUrn(s)
		u
	}

	def updateCurrentListOfUrns(c:Corpus):Unit = {
		O2Model.currentListOfUrns.value.clear
		for (n <- c.nodes){
			O2Model.currentListOfUrns.value += n.urn
		}	
	}


	def updateCurrentCorpus(c:Corpus, u:CtsUrn):Unit = {
		try {
			O2Model.currentCorpus.value.clear
			if (O2Model.hasTextRepo.value) {
				// Since GroupBy doesn't preserve order, let's preserve our own order
				val versionLevelOrder:Vector[CtsUrn] = {
					c.urns.map(u => dropOneLevel(u)).distinct.toVector
				}
				// Get Corpus into a Vector of tuples: (version-level-urn, vector[CitableNode])
				val tempCorpusVector:Vector[(CtsUrn, Vector[CitableNode])] = c.nodes.groupBy(_.urn.dropPassage).toVector
// in correct order to this point				
				for (tc <- tempCorpusVector) {
					val versionLabel:String = {
						O2Model.currentCatalog.value match {
							case Some(c) => c.label(tc._1)
							case None => "No catalog found!"
						}
					}
					val passageString:String = {
						u.passageComponent
					}
// in correct order to this point				
					val boundVersionLabel = Var(versionLabel)

					val versionUrn:CtsUrn = CtsUrn(s"${tc._1}${passageString}")
					val boundVersionUrn = Var(versionUrn)

					// Group node urns according to nodeBlocks
					val nodeBlocks:Vector[(CtsUrn, Vector[CitableNode])] = {
						tc._1.exemplarOption match {
							case Some(eo) => {
								val block:Vector[(CtsUrn,Vector[CitableNode])] = {
									tc._2.groupBy(n => dropOneLevel(n.urn)).toVector	
								}
								val block2 = tc._2.zipWithIndex.groupBy(n => dropOneLevel(n._1.urn))
								val lhm = LinkedHashMap(block2.toSeq sortBy (_._2.head._2): _*)
								val block3 = lhm mapValues (_ map (_._1))
								val sortedBlock = block3.toVector
								sortedBlock
							}
							case None => {
								Vector( (tc._1,tc._2)	)
							}
						}	
					}	
					// Get this nodeBlock into a versionNodeBlock
					val tempNodeBlockVec = Vars.empty[VersionNodeBlock]	
					for (b <- nodeBlocks){
						val tempBlockUrn = Var(b._1)
						val tempNodesVec = Vars.empty[CitableNode]
						for (n <- b._2) tempNodesVec.value += n
						tempNodeBlockVec.value += VersionNodeBlock(tempBlockUrn, tempNodesVec)
					}

					O2Model.currentCorpus.value += BoundCorpus(boundVersionUrn, boundVersionLabel, tempNodeBlockVec)
				}	
			}

		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"O2Model Exception in 'updateCurrentCorpus': ${e}",2)
			}
		}
	}

	def dropOneLevel(u:CtsUrn):CtsUrn = { 
		try {
			val passage:String = u.passageComponent
			val plainUrn:String = u.dropPassage.toString
			val newPassage:String = passage.split('.').dropRight(1).mkString(".")
			val droppedUrn:CtsUrn = CtsUrn(s"${plainUrn}${newPassage}") 
			droppedUrn
		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"Error dropping one level from ${u}: ${e}",2)
				throw new Exception(s"${e}")
			}
		}
	}

	def displayNewPassage(urn:CtsUrn):Unit = {
		O2View.cursorWaiting
		val task2 = Task{ CiteMainQuery.getJson(O2Query.getCorpus, s"${O2Query.queryGetCorpus}${urn}", urn = Some(urn))}
		val future2 = task2.runAsync
	}

	@dom
	def clearPassage:Unit = {
		O2Model.versionsForCurrentUrn.value = 0
		O2Model.currentListOfUrns.value.clear
		O2Model.currentCorpus.value.clear
	}

	def passageLevel(u:CtsUrn):Int = {
		try {
			val vi:Vector[Int] = u.citationDepth
			vi.size match {
				case n if (n == 1 ) => vi(0)
				case n if (n == 2) => throw new Exception(s"Cannot report passage level for ${urn} becfause it is a range.")
				case _ => throw new Exception(s"Cannot report passage level for ${u} because it does not have a passage component.")
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")	
		}
	} 

	def valueForLevel(u:CtsUrn,level:Int):String = {
		try {
			val urn = u.dropSubref
			val pl:Int = passageLevel(urn)
			if (pl < level) throw new Exception(s"${u} has a ${pl}-deep citation level, which is less than ${level}.")
			urn.passageComponent.size match {
				case n if (n > 0) => {
					urn.passageComponent.split('.')(level-1)
				}
				case _ => throw new Exception(s"Cannot report passage level for ${u} because it does not have a passage component.")
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")	
		}	
	}

/*
	@dom
	def displayPassage(newUrn: CtsUrn):Unit = {
		val tempCorpus: Corpus = O2Model.textRepo.value.get.corpus >= newUrn
		O2Model.updateCurrentListOfUrns(tempCorpus)
		DSEModel.updateCurrentListOfDseUrns(tempCorpus)
		CommentaryModel.updateCurrentListOfComments(tempCorpus)
		O2Model.updateCurrentCorpus(tempCorpus, newUrn)
		O2Model.currentNumberOfCitableNodes.value = tempCorpus.size
	}
	*/



def checkForRTL(s:String):Boolean = {
		val sStart = s.take(10)
		val arabicBlock = "[\u0600-\u06FF]".r
		val hebrewBlock = "[\u0591-\u05F4]".r
		var isRtl:Boolean = ((arabicBlock findAllIn sStart).nonEmpty || (hebrewBlock findAllIn sStart).nonEmpty)
		isRtl
}



	@dom
	def updateCitedWorks = {
		O2Model.citedWorks.value.clear
		O2Model.currentCatalog.value = None
		val task = Task{ CiteMainQuery.getJson(O2Query.updateCatalog, O2Query.queryCatalog, urn = None) }
		val future = task.runAsync
	}


}
