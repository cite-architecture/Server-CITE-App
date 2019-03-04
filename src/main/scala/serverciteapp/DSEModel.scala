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
import edu.holycross.shot.scm._
import edu.holycross.shot.dse._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("DSEModel")
object DSEModel {

	val dsesForCurrentText = Vars.empty[DseRecord]
	val dsesForCurrentObjects = Vars.empty[DseRecord]
	val dsesForCurrentImage = Vars.empty[DseRecord]

	def clearDsesForCurrentText:Unit    =  dsesForCurrentText.value.clear 
	def clearDsesForCurrentObjects:Unit = dsesForCurrentObjects.value.clear 
	def clearDsesForCurrentImage:Unit   = dsesForCurrentImage.value.clear 


	def updateDsesForCurrentText(dseVec:Vector[DseRecord]) = {
		dsesForCurrentText.value.clear
		for (dr <- dseVec) { dsesForCurrentText.value += dr }
	}
	def updateDsesForCurrentObjects(dseVec:Vector[DseRecord]) = {
		dsesForCurrentObjects.value.clear
		for (dr <- dseVec) { dsesForCurrentObjects.value += dr }
	}
	def updateDsesForCurrentImage(dseVec:Vector[DseRecord]) = {
		dsesForCurrentImage.value.clear
		for (dr <- dseVec) { dsesForCurrentImage.value += dr }
	}

	def ctsInDse(urn:CtsUrn):Vars[Cite2Urn] = {
 		dsesForCurrentText.value.size match {
 			case n if (n < 1) => Vars.empty[Cite2Urn]
 			case _ => {
 				val filterList:Vector[Cite2Urn] = {
	 				DSEModel.dsesForCurrentText.value.filter( du => {
	 					du.passage == urn
	 				}).map(_.citeObject.urn).toVector
	 			}
	 			filterList.size match {
	 				case 0 => Vars.empty[Cite2Urn]
	 				case _ => {
	 					val v = Vars.empty[Cite2Urn]
	 					for (f <- filterList) {
	 						v.value += f
	 					}
	 					v
	 				}
	 			}
 			}
 		}	
 	}

 	def roisForImage(urn:Cite2Urn, contextUrn:Option[Cite2Urn], dseUrns:Option[Vector[Cite2Urn]]):Option[Vector[ImageRoiModel.Roi]] = {

 		try {
	 		// If there is an ROI already on the URN, and a contextUrn, make an ROI object for those
			val originalRoi:Option[Vector[ImageRoiModel.Roi]] = {
				val tempRoi:Option[ImageRoiModel.Roi] = ImageRoiModel.roiFromUrn(urn, data = contextUrn, context = contextUrn)
				tempRoi match {
					case Some(r) => Some(Vector(r))
					case None => None
				}
			}

			//Get an ROI for the surface (there should be only one)

			// Get ROI objects for all objects in dseUrns mapped to this image
			val allRois:Option[Vector[ImageRoiModel.Roi]] = {

				dseUrns match {
					case None => None
					case Some(urns) => {
						val dseRecs:Vector[DseRecord] = urns.map(u => {
							val recs:Vector[DseRecord] =  dsesForCurrentObjects.value.filter(dseRec => {
								dseRec.citeObject.urn == u
							}).toVector
							recs
						}).toVector.flatten
						if (dseRecs.size < 0) throw new Exception(s"DSEModel: unaccountably failed to find matches for ${urn}")	

						val roiVec:Vector[Option[ImageRoiModel.Roi]] = dseRecs.map( dr => {
							val roiUrn:Cite2Urn = dr.imageroi
							val textUrn:CtsUrn = dr.passage
							val thisObject:CiteObject = dr.citeObject
							val thisRoiObject:Option[ImageRoiModel.Roi] = ImageRoiModel.roiFromUrn(roiUrn, data = Some(textUrn), context = Some(thisObject.urn))
							thisRoiObject
						}).toVector
						val realRois:Vector[ImageRoiModel.Roi] = roiVec.filter(_ != None).map(_.get)
						realRois.size match {
							case n if (n == 0) => None
							case _ => Some(realRois)
						}
					}
				}
			}


			val totalRois:Option[Vector[ImageRoiModel.Roi]] = {
				originalRoi match {
					case None => {
						allRois match {
							case None => None
							case Some(arois) => Some(arois)
						}
					}
					case Some(oroi) => {
						allRois match {
							case None => Some(oroi)
							case Some(arois) => Some(oroi ++ arois)
						}
					}
				}
			}
			totalRois
		} catch {
			case e:Exception => throw new Exception(s"DSE Model: ${e}.")
		}
 	}

}
