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
import scala.scalajs.js.Dynamic.{ global => g }
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("NGModel")
object NGModel {

	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

	var corpusOrUrn = Var[Option[CtsUrn]](None)

	val urn = Var(CtsUrn("urn:cts:ns:group.work.version.exemplar:passage"))

	val shortWorkLabel = Var("default")

	val citedWorks = Vars.empty[CtsUrn]
	val currentCatalog = Var[Option[Catalog]](None)
	val hasTextRepo = Var(false)

	val userMessage = Var("")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")

	/* holding for queries, in this async world */	
	val currentStringSearchResults = Var[Option[Corpus]](None)

	/* For recording queries */

	class CtsQuery( urn:Option[CtsUrn]) {
		override def toString:String = {
			val s:String = s"Generic query on ${urn}."
			s
		}
	}

	case class NGramQuery(n:Integer, t:Integer, fs: String, ip: Boolean, urn:Option[CtsUrn] ) extends CtsQuery(urn){
		// this.getClass.getName will yield 'citeapp.NGModel$NGramQuery'
		override def toString:String = {
			val scope:String = urn match{ case Some(u) => u.toString; case _ => "whole corpus"}
			val s = s"""NGram Query: n=${n}; threshold = ${t}; ignore-punctuation=${ip}; filtered by "${fs}"; scope=${ scope }."""
			s
		}

	}

	case class StringSearch(fs: String, urn:Option[CtsUrn] ) extends CtsQuery(urn) {
		override def toString:String = {
			val scope:String = urn match{ case Some(u) => u.toString; case _ => "whole corpus"}
			val s = s"""String Search: "${fs}"; scope=${ scope }."""
			s
		}

	}


	case class TokenSearch(tt: Vector[String], p:Integer, urn:Option[CtsUrn]) extends CtsQuery(urn){
		override def toString:String = {
			val scope:String = urn match{ case Some(u) => u.toString; case _ => "whole corpus"}
			val s = s"""Token Search: "${tt.mkString(", ")}"; proximity=${p}; scope=${ scope }."""
			s
		}
	}

	val pastQueries = Vars.empty[CtsQuery]

	/* for holding search results */
  case class SearchResult(urn: Var[CtsUrn], kwic: Var[String])
	val citationResults =  Vars.empty[SearchResult]

	/* Values for NGrams */
	val nGramThreshold = Var(3)

	val nGramResults = Vars.empty[StringCount]
	
	val nGramQueryReport = Var("")
	val otherQueryReport = Var("")

	/* Values for Search */
	val tokenSearchProximity = Var(20)

	/* Some methods for working the model */

	@dom
	def updateShortWorkLabel = {
		/*
		if ( O2Model.textRepo.value == None){
					NGModel.shortWorkLabel.value = "- no selected text -"
		} else {
			val longS:String = O2Model.textRepo.value.get.catalog.label(NGModel.urn.value)
			if (longS.size > 50){
				val shortS:String = longS.take(24) + " … " + longS.takeRight(23)
				NGModel.shortWorkLabel.value = shortS
			} else {
				NGModel.shortWorkLabel.value = longS
			}
		}
		*/
	}

/*
	@dom
	def updateCitedWorks = {
		NGModel.citedWorks.value.clear
		NGController.clearResults
		NGController.clearInputs
		// N.b. The textRepository remains with the Ohco2 Model.
		for ( cw <- O2Model.textRepo.value.get.corpus.citedWorks){
			NGModel.citedWorks.value += cw
		}
	}
	*/


	/* NGram Searching */

/*
 def getNGram(filterString: String, n: Int, occ: Int, ignorePunc: Boolean ): StringHistogram = {
		 getNGram(O2Model.textRepo.value.get.corpus, filterString, n, occ, ignorePunc)
 }
 */


/* 
def getNGram(ngUrn: CtsUrn, filterString: String, n: Int, occ: Int, ignorePunc: Boolean): StringHistogram = {
	 val newCorpus: Corpus = O2Model.textRepo.value.get.corpus ~~ ngUrn
	 getNGram(newCorpus, filterString, n, occ, ignorePunc)
 }
*/

 def getNGram(ngCorpus:Corpus, filterString: String, n: Int, occ: Int, ignorePunc: Boolean ): StringHistogram = {

		val hist:StringHistogram = {
			if( filterString == ""){
				ngCorpus.ngramHisto(n, occ, ignorePunc)
			} else {
				ngCorpus.ngramHisto(filterString, n, occ , ignorePunc)
			}
		}
		hist
	}

/*
 def getUrnsForNGram(s: String, ignorePunc: Boolean ): Vector[CtsUrn] ={
	 val vurn = O2Model.textRepo.value.get.corpus.urnsForNGram(s, 1, ignorePunc)
	 vurn
 }

*/

/*
 def getUrnsForNGram(ngUrn: CtsUrn, s: String, ignorePunc: Boolean ): Vector[CtsUrn] ={
	 val newCorpus: Corpus = O2Model.textRepo.value.get.corpus >= ngUrn
	 val vurn = newCorpus.urnsForNGram(s, 1, ignorePunc)
	 vurn
 }

*/

/*
 def getUrnsForNGram(ngCorpus: Corpus, s: String, ignorePunc: Boolean ): Vector[CtsUrn] = {
	 	val vurn = ngCorpus.urnsForNGram(s, 1, ignorePunc)
		vurn
 }

*/

/* String and Token Finding */


def findString(s:String, urn:Urn):Unit = {
		val task = Task{ CiteMainQuery.getJson(NGQuery.getFindString, s"${NGQuery.queryFindString}/${urn}", urn = Some(urn)) }
		val future = task.runAsync	
}


def findString(s:String):Unit = {
		val task = Task{ CiteMainQuery.getJson(NGQuery.getFindString, s"${NGQuery.queryFindString}", urn = None) }
		val future = task.runAsync	
}


}
