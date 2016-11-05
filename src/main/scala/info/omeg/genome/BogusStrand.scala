package info.omeg.genome

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 01.11.16.
  */
class BogusStrand(elem: Int) extends GeneStrand {
  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    if(rateRemove)
      return

    if(rateModify)
      lb += (elem + (math.random * 10 - 5)).toInt
    else
      lb += elem
  }

  override def getMarker: Int = -1

  override def getId(): Int = elem

  override def isRemove: Double = super.isRemove * 5
  override def isModify: Double = super.isModify * 10
  override def isAppend: Double = super.isAppend * 2
}
