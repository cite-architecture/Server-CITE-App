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

@JSExportTopLevel("serverciteapp.DataModelView")
object DataModelView {

@dom
def objectLinks(contextUrn:Option[Cite2Urn], propVal:Cite2Urn) = {
	propVal.objectComponentOption match {
		case Some(oc) => {
			<ul class="citeLinks_linksList">
				{ CiteBinaryImageView.imageThumbItem(propVal, CiteBinaryImageModel.imgUseLocal.bind).bind }
				{ DataModelView.objectLinkItem(contextUrn, propVal).bind }
				{ DataModelView.imageLinkItem(contextUrn, propVal).bind }
				{ DataModelView.dseLinkItem(contextUrn, propVal).bind }
				{ DataModelView.relationsLinkItem(contextUrn, propVal).bind }
			</ul>
		}
		case None => { 
			<ul class="citeLinks_linksList">
				{ DataModelView.objectLinkItem(contextUrn, propVal).bind }
			</ul>
		}
	}
}

@dom
def textLinks(contextUrn:Option[Cite2Urn], u:CtsUrn) = {
	<ul class="citeLinks_linksList">
		{ DataModelView.textLinkItem(contextUrn, u).bind }
	</ul>
}

@dom
def textLinkItem(contextUrn:Option[Cite2Urn], u:CtsUrn, idString:String = "", groupId:String = "") = {
	val groupClass:String = s"citeLinks_linkItem image_roiGroup_${groupId}"
	DataModelController.hasText(u) match {
		case true => {
			<li class={groupClass} id={idString}>
				<a
					onclick={ event: Event => {
						val mouseEvent = event.asInstanceOf[MouseEvent]
						if (mouseEvent.metaKey){
							true
						} else {
							DataModelController.retrieveTextPassage(contextUrn, u)
							false
						}
					}}
					href={ s"?urn=${u}" }
					>
					{u.toString}
				</a>
			</li>
		}
		case _ => {
			<li class={groupClass}>
					<span id={idString}>
					{u.toString} </span><br/> 
					[text not in this library]
			</li>
		}
	}
}


@dom
	def objectLinkItem(contextUrn:Option[Cite2Urn],propVal:Cite2Urn,labeled:Boolean = false, idString:String = "", groupId:String = "") = {

			val tempUrn:Cite2Urn = {
				propVal.versionOption match {
					case Some(v) => propVal.dropExtensions.dropProperty
					case None => propVal.dropExtensions
				}
			}

			val roiGroupClass:String = {
				groupId match {
					case s if (s == "") => ""
					case _ => s"image_roiGroup_${groupId}"
				}
			}
			val groupClass:String = s"citeLinks_linkItem ${roiGroupClass}"
			DataModelController.hasObject(tempUrn) match {
				case true => {
					<li class={groupClass} id={idString}>
						<a onclick={ event: Event => {
							val mouseEvent = event.asInstanceOf[MouseEvent]
							if (mouseEvent.metaKey){
								true
							} else {
								DataModelController.retrieveObject(contextUrn,tempUrn)
								false
							}
						}
					} 
					href={ s"?urn=${tempUrn}" }
					> { 
						labeled match {
							case true => {
								s"${ObjectModel.labelMap.value.get(propVal.dropExtensions.dropProperty)}" 
							}
							case _ => {
								"View Item"
							}
						}
					}</a></li>
				}
				case _ => {
					// need to see if it is a collection or range
					var isBrowsable:Boolean = {
						( propVal.isRange ) || 
						( propVal.objectComponentOption == None)
					}
					isBrowsable match {
						case false => {
							labeled match {
								case true => { <li class={groupClass}>{ s"Object ${tempUrn} not in Library" }</li> }
								case _ => { <li><span id={idString}>Object not in Library</span></li> }
							}
						}
						case _ => {
							val coll:Option[CiteCollectionDef] = {
								ObjectModel.currentCatalog.value.get.collection(propVal)
							}
							coll match {
								case Some(c) => {
									<li class={groupClass} id={idString}>
										<a onclick={ event: Event => {
											val mouseEvent = event.asInstanceOf[MouseEvent]
											if (mouseEvent.metaKey){
												true
											} else {
												DataModelController.retrieveObject(contextUrn,tempUrn)
												false
											}
										}
									} 
									href={ s"?urn=${tempUrn}" }
									> { 
									labeled match {
										case true => {  
											val l:String = c.collectionLabel
											l
										}
										case _ => { "Browse Collection" }
									}

									}</a></li>
								}
								case None => {
									<li class={groupClass} id={idString}>
										{ "Collection not in library." }
									</li>
								}
							}
							
						}
					}
				}
			}
	}	

	@dom
	def imageLinkItem(contextUrn:Option[Cite2Urn],propVal:Cite2Urn) = {
		CiteBinaryImageController.implementedByImageCollObjects(propVal) match {
			case Some(uv) => {
				<li class="citeLinks_linkItem">
					{ DataModelView.iipDZLink(propVal, 
						uv, 
						contextUrn, 
						roiObject = {
							val tr = ImageRoiModel.roiFromUrn(propVal, contextUrn, contextUrn )
							val optVecRoi:Option[Vector[ImageRoiModel.Roi]] = {
								tr match {
									case Some(r) => Some(Vector(r))
									case None => None
								}
							}
							optVecRoi
						}
						).bind }
				</li>
			}
			case None => {
				 <!-- empty content -->	
			}
		}
	}	

	@dom
	def dseLinkItem(contextUrn:Option[Cite2Urn],propVal:Cite2Urn) = {
		// First, see if this is a binary image
		CiteBinaryImageController.implementedByImageCollObjects(propVal) match {
			case Some(uv) => {
				//Then, see if it is represented in DSE
				//val dseUrns:Option[Vector[Cite2Urn]] = DSEModel.implementedByDSE_image(propVal)
				val dseUrnsVec:Vector[Cite2Urn] = DSEModel.dsesForCurrentObjects.value.filter(d => d.imageroi.dropExtensions == propVal.dropExtensions).map(_.citeObject.urn).toVector
				val dseUrns:Option[Vector[Cite2Urn]] = {
					dseUrnsVec.size match {
						case n if (n < 1) => None
						case _ => Some(dseUrnsVec)
					}
				}
				// If there is an existing ROI, add that to allRois
				val allRois:Option[Vector[ImageRoiModel.Roi]] = {
					val dseRois:Option[Vector[ImageRoiModel.Roi]] = DSEModel.roisForImage(propVal, contextUrn, dseUrns)
					dseRois
				}

				<span class="citeLinks_linkSpan"> 
					Data Mapped to:
					{ DataModelView.iipDZLink(propVal.dropExtensions, uv, contextUrn, roiObject = allRois).bind }
				</span>
			}
			case None => { <!-- empty content --> }
		}
	}

	@dom
	def iipDZLink(urn:Cite2Urn, uv:Vector[Cite2Urn], contextUrn:Option[Cite2Urn], roiObject:Option[Vector[ImageRoiModel.Roi]] = None) = {
		CiteBinaryImageController.implementedByProtocol(uv,CiteBinaryImageModel.iiifApiProtocolString) match {
			case Some(co) => {
				CiteBinaryImageModel.imgUseLocal.bind match {
					case false => {
						<span class="citeLinks_linkSpan">
							<a onclick={ event: Event => {
								val roisToPaint:Option[Vector[ImageRoiModel.Roi]] = roiObject match {
									case None => {
											val roi:Option[Vector[ImageRoiModel.Roi]] = {
												val tempRoi:Option[ImageRoiModel.Roi] = ImageRoiModel.roiFromUrn(urn, contextUrn)
												CiteBinaryImageModel.currentContextUrn.value = contextUrn
												tempRoi match {
													case Some(r) => Some(Vector(r))
													case None => None
												}
											}
											roi
										}
									case Some(rois) => {
										Some(rois)
									}
								}
								val mouseEvent = event.asInstanceOf[MouseEvent]
								if (mouseEvent.metaKey){
									true
								} else {
									DataModelController.viewImage(contextUrn, co, urn, roisToPaint)
									false
								}
							}
						} 
						href={ s"?urn=${urn}" }
						>Remote Image</a></span>
					}
					case _ => {
						<!-- empty content -->
					}
				}
			}
			case None => {
				<!-- empty content -->							
			}
		}
	}


	

	@dom
	def mappedDataToTextContainer = {
		O2Model.hasTextRepo.bind match {
			case false => {
				<p>No Texts</p>	
			}
			case _ => {
				<div>
				{ mappedDseToTextContainer.bind }
			 { mappedCommentaryToTextContainer.bind }
				</div>
			}
		}

	}

	@dom
	def mappedDseToTextContainer = {
		<div id="o2_mappedDseContainer" class={
				DSEModel.dsesForCurrentText.length.bind match {
					case s if (s > 0) => "app_visible"
					case _ => "app_hidden"
				}	
			}>
			<h2>DSE Mapped Passages</h2>
			{ mappedDsePassages.bind }
		</div>
	}

	@dom
	def mappedCommentaryToTextContainer = {
		<div id="o2_mappedCommentContainer" class={
				CommentaryModel.currentCommentsAll.length.bind match {
					case s if (s > 0) => "app_visible"
					case _ => "app_hidden"
				}	
			}>
			<h2>Commentaries</h2>
			{ commentaryPassages.bind}
		</div>	
	}

	@dom
	def commentaryTextLinkItem(u:CtsUrn) = {
		<a
			onclick={ event: Event => {
				val mouseEvent = event.asInstanceOf[MouseEvent]
				if (mouseEvent.metaKey){
					true
				} else {
					DataModelController.retrieveTextPassage(None, u)
					false
				}
			}}
			href={ s"?urn=${u}" }
			>
			{u.toString}
		</a>
	}

	@dom
	def commentaryObjectLinkItem(urn:Cite2Urn) = {
		<a onclick={ event: Event => {
				val mouseEvent = event.asInstanceOf[MouseEvent]
				if (mouseEvent.metaKey){
					true
				} else {
					DataModelController.retrieveObject(None,urn)
					false
				}
			}
		} 
		href={ s"?urn=${urn}" }
		> { urn.toString }</a>
	}

	@dom
	def commentaryLinkItem(c:CommentaryModel.CiteComment) = {
		<li>
			{
				c.comment match {
					case CtsUrn(_) => commentaryTextLinkItem(c.comment.asInstanceOf[CtsUrn]).bind
					case _ => commentaryObjectLinkItem(c.comment.asInstanceOf[Cite2Urn]).bind
				}	
			}	
			"comments on"
			{
				c.text match {
					case CtsUrn(_) => commentaryTextLinkItem(c.text.asInstanceOf[CtsUrn]).bind
					case _ => commentaryObjectLinkItem(c.text.asInstanceOf[Cite2Urn]).bind
				}	
			}	

		</li>
	}


	@dom
	def commentaryPassages = {
		O2Model.currentNumberOfCitableNodes.bind match {
			case 0 => { <p>None</p> }
			case _ => {  
				<ul>{ 
					for (c <- CommentaryModel.currentCommentsDistinctComments) yield {
						g.console.log(s"got commentary: ${c}")
						
						commentaryLinkItem(c).bind
					}
				} </ul>
			}
		}	
	}
	

	@dom
	def mappedDsePassages = {
		O2Model.currentNumberOfCitableNodes.bind match {
			case 0 => { <p>No DSE Passages</p> }
			case _ => {  
				val currentUrn:CtsUrn = O2Model.urn.bind
				<ul>{ 
					for (u <- DSEModel.dsesForCurrentText) yield {
						{ DataModelView.objectLinkItem(None, u.citeObject.urn, true).bind }
					}
				} </ul>
			}
		}	
	}
 	
	@dom
	def relationsLinkItem(contextUrn:Option[Cite2Urn],propVal:Cite2Urn) = {
		<span class="citeLinks_linkSpan"> 
			<a onclick={ event: Event => {
					js.Dynamic.global.document.getElementById("relations_urnInput").value = propVal.toString
					CiteMainView.changeTab("relations")
					RelationsController.changeUrn(s"${propVal.toString}")
				}
			}>Find Relations</a>
			
		</span>
	}

}