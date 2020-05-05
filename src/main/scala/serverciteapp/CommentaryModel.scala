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
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.scm._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("CommentaryModel")
object CommentaryModel {

	case class CiteComment(comment:Urn, text:Urn)

	val commentaryVerb:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:commentsOn")
	val commentaryModel:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:commentarymodel")
	// commentsOn not used yet…
	val commentsOn = Var[Boolean](true)


	val commentList = Vars.empty[CiteComment]
	val currentCommentsAll = Vars.empty[CiteComment]
	val currentCommentsDistinctComments = Vars.empty[CiteComment]

	def ctsHasCommentary(urn:CtsUrn):Vars[Urn] = {
		DataModelController.hasCommentaryModel match {
			case false => Vars.empty[Urn]	
			case _ => {
				val relevantComments:Vector[CiteComment] = currentCommentsAll.value.filter(_.text.asInstanceOf[CtsUrn] == urn).toVector
				val v = Vars.empty[Urn]	
				for (c <- relevantComments) {
					v.value += c.comment
				}
				v
			}
		}
	}

	def clearComments:Unit = {
		commentList.value.clear
		currentCommentsAll.value.clear
		currentCommentsDistinctComments.value.clear
	}

	def updateCurrentListOfComments(commentVec:Vector[CiteTriple]):Unit = {
		clearComments	
		for (c <- commentVec) currentCommentsAll.value += CiteComment(c.urn1, c.urn2)	
		val finalCurrentComments:Vector[CiteComment] = commentVec.map(c => CiteComment(c.urn1, c.urn2))

		// And let's get a version that just has unique comments, for the sidebar
		val uniquedComments:Vector[CiteComment] = finalCurrentComments.groupBy(_.comment).map(_._2.head).toVector
		// And why not group by work/collection, while we're at it… the list isn't going to be long
		val map1:Vector[Tuple2[String,CiteComment]] = uniquedComments.map(c => {
			val mapString:String = {
				c.comment match {
					case CtsUrn(_) => c.comment.asInstanceOf[CtsUrn].dropPassage.toString
					case _ => c.comment.asInstanceOf[Cite2Urn].dropSelector.toString
				}
			}
			Tuple2(mapString,c)
		})
		val map2:Vector[(String,Vector[Tuple2[String,CiteComment]])] = map1.groupBy(_._1).toVector
		val map3:Vector[Tuple2[String,CiteComment]] = map2.map(_._2).flatten
		val map4:Vector[CiteComment] = map3.map(_._2)

		for (c <- map4){
			currentCommentsDistinctComments.value += c
		}
	}

	


}
