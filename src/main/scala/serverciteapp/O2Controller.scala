package serverciteapp

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.concurrent._
import scala.scalajs.js.Dynamic.{ global => g }
//import ExecutionContext.Implicits.global
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("serverciteapp.O2Controller")
object O2Controller {


	val validUrnInField = Var(false)


	/* A lot of work gets done here */
	def changePassage: Unit = {
		O2View.cursorWaiting
		O2Model.getPrevNextUrn(O2Model.urn.value)
		val newUrn: CtsUrn = O2Model.urn.value

		val task = Task{ CiteMainQuery.getJson(O2Query.getLabelForUrnHistory, s"${O2Query.queryLabelForUrn}${newUrn}", urn = Some(newUrn)) }
		val future = task.runAsync	
		O2Model.versionsForUrn(newUrn)
		val task2 = Task{ CiteMainQuery.getJson(O2Query.getCorpus, s"${O2Query.queryGetCorpus}${newUrn}", urn = Some(newUrn))}
		val future2 = task2.runAsync
	}


	def updateUserMessage(msg: String, alert: Int): Unit = {
		O2Model.userMessageVisibility.value = "app_visible"
		O2Model.userMessage.value = msg
		alert match {
			case 0 => O2Model.userAlert.value = "default"
			case 1 => O2Model.userAlert.value = "wait"
			case 2 => O2Model.userAlert.value = "warn"
		}
		js.timers.clearTimeout(O2Model.msgTimer)
		O2Model.msgTimer = js.timers.setTimeout(6000){ O2Model.userMessageVisibility.value = "app_hidden" }
	}


	def validateUrn(urnString: String): Unit = {
		try{
			val newUrn: CtsUrn = CtsUrn(urnString)
			validUrnInField.value = true
		} catch {
			case e: Exception => {
				validUrnInField.value = false
			}
		}
	}

	def getNext:Unit = {
		if (O2Model.currentNext.value != None){
			changeUrn(O2Model.currentNext.value.get)
		}
	}

	def getPrev:Unit = {
		if (O2Model.currentPrev.value != None){
			changeUrn(O2Model.currentPrev.value.get)
		}
	}

	
	def changeUrn(urnString: String): Unit = {
		changeUrn(CtsUrn(urnString))
	}
	

	
	def changeUrn(urn: CtsUrn): Unit = {
		try {
			O2Model.urn.value = urn
			O2Model.displayUrn.value = urn
			validUrnInField.value = true
			O2Controller.updateUserMessage("Retrieving passage…",1)
			O2Controller.changePassage

		} catch {
			case e: Exception => {
				validUrnInField.value = false
				updateUserMessage("Invalid URN. Current URN not changed.",2)
			}
		}
	}
	

	
	def insertFirstNodeUrn(urn: CtsUrn): Unit = {
		val task = Task{ CiteMainQuery.getJson(O2Query.getFirstUrn, s"${O2Query.queryFirstUrn}${urn}") }
		val future = task.runAsync	
	}



	@dom
	def preloadUrn = {
		if (O2Model.citedWorks.value.length > 0) {
			val firstWork:CtsUrn = O2Model.citedWorks.value(0)
			val task = Task{ CiteMainQuery.getJson(O2Query.getFirstUrn, s"${O2Query.queryFirstUrn}${firstWork.toString}", urn = None) }
			val future = task.runAsync	
		}
	}



}
