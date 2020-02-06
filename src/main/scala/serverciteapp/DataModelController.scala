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

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("DataModelController")
object DataModelController {

	// Clear Data Models
	def clearDataModels:Unit = {
		DataModelModel.dataModels.value = None
		CiteBinaryImageModel.hasBinaryImages.value = false
		CiteBinaryImageModel.hasRemoteOption.value = false
		CiteBinaryImageModel.hasLocalOption.value = false
	}

 // Probably should be in CiteObj library?
  // Given a collection URN and a property name, construct a property URN
  def propertyUrnFromPropertyName(urn:Cite2Urn, propName:String):Cite2Urn = {
    //println("\n\n-------")
    //println(s"urn: ${urn}")
    val returnUrn:Cite2Urn = {
        val collUrn:Cite2Urn = {
            urn.propertyOption match {
            case Some(po) => {
              urn.dropProperty.dropSelector
            }
            case None => {
              urn.dropSelector
            }
          }
        }
        //println(s"collUrn: ${collUrn}")
        val collUrnString:String = collUrn.toString.dropRight(1) // remove colon
        urn.objectComponentOption match {
        case Some(oc) => {
          Cite2Urn(s"${collUrnString}.${propName}:${oc}")
        }
        case None => {
          Cite2Urn(s"${collUrnString}.${propName}:")
        }
      }
    }
    //println(s"returnUrn: ${returnUrn}")
    returnUrn
  } 

	// Checks to see if a text is present in the currently loaded library
	// Will match with ~~ similarity
	// *** ALERT *** for testing, I'm turning all of these into work-level URNs.
	def hasText(u:CtsUrn):Boolean = {
		O2Model.hasTextRepo.value match {
			case true => {
				O2Model.currentCatalog.value.get.texts.filter(_.urn ~~ u.dropPassage).size match {
					case 0 => false
					case _ => true
				}
			}
			case false => {
				false		
			}
		}
	}

	/* Check to see if the Commentary datamodel is present */
	def hasCommentaryModel:Boolean = {
		val commUrn:Cite2Urn = CommentaryModel.commentaryModel	
		DataModelModel.dataModels.value match {
			case None => {
				false
			}
			case Some(dms) => {
				val implementations:Vector[DataModel] = dms.filter(_.model == commUrn).toVector
				implementations.size match {
					case 0 => false
					case _ => true
				}	
			}
		}
	}

	def hasObject(u:Cite2Urn):Boolean = {
		try {
			val urn:Cite2Urn = {
				val tempU:Cite2Urn = u.dropExtensions	
				tempU.isRange match {
					case true => tempU.rangeBeginUrn
					case _ => tempU
				}
			}
			ObjectModel.labelMap.value match {
				case Some(lm) => {
					lm.contains(urn)
				}
				case None => {
					false		
				}
			}	
		} catch {
			case e:Exception => false
		}
	}

	/* Check to see if the Citable Image datamodel is:
			1. supported by this app
			2. present in this library
			3. implemented by the collection represented by `u`
	*/
	def isCitableImage(u:Cite2Urn):Boolean ={
		val collUrn:Cite2Urn = u.dropSelector
		val citableImageModelUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:imagemodel")

		DataModelModel.dataModels.value match {
			case None => false
			case Some(dms) => {
				val implementations:Vector[DataModel] = dms.filter(_.model == citableImageModelUrn).filter(_.collection == collUrn)	
				implementations.size match {
					case 0 => false
					case _ => true
				}	
			}
		}
	}

/*
	Given a Cite2Urn, see if it identifies a property that is of type 'text' and is
	extended by a published protocol
	*/
	// Returns None, or the type of extension
	def textPropertyIsExtended(propUrn:Cite2Urn):Option[String] = {
		DataModelModel.extendedTextProperties.value match {
			case None => None
			case Some( vecTups ) => {
				vecTups.filter( _._1 == propUrn.dropSelector).size match {
					case n if (n < 1 ) => None
					case _ => {
						val s = vecTups.filter( _._1 == propUrn.dropSelector).head._2
						Some(s)
					}
				}
			}
		}	
	}


 	/*
	Methods for switching tabs and loading text and objects
 	*/

 	def retrieveUrn(u:Urn):Unit = {
 		u match {
 			case CtsUrn(_) => retrieveTextPassage(None, u.asInstanceOf[CtsUrn])
 			case Cite2Urn(_) => {
 				retrieveObject(None, u.asInstanceOf[Cite2Urn])
 			}
 			case _ => 	CiteMainController.updateUserMessage(s"Could not resolve ${u} to either CtsUrn or Cite2Urn",2)

 		}
 	}

	def retrieveTextPassage(contextUrn:Option[Cite2Urn] = None, urn:CtsUrn):Unit = {
			O2Controller.changeUrn(urn)
			CiteMainView.changeTab("text")
	}

	def retrieveObject(contextUrn:Option[Cite2Urn] = None, urn:Cite2Urn):Unit = {
			val tempUrn:Cite2Urn = urn.dropExtensions
			ObjectController.changeUrn(tempUrn)
			CiteMainView.changeTab("object")
	}

	def viewImage(
		contextUrn:Option[Cite2Urn], 
		implementingObject:CiteObject, 
		urn:Cite2Urn, 
		roiObj:ImageRoiModel.Roi
	):Unit = {
		viewImage(contextUrn, implementingObject, urn, Some(Vector(roiObj)))
	}

	def viewImage(
		contextUrn:Option[Cite2Urn], 
		implementingObject:CiteObject, 
		urn:Cite2Urn, 
		roiObj:Option[Vector[ImageRoiModel.Roi]]
	):Unit = {
			CiteBinaryImageController.changeUrn(contextUrn, urn, roiObj)
			CiteMainView.changeTab("image")
		}


}
