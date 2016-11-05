package info.omeg.genome

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 01.11.16.
  */
trait GeneStrand {
  def writeSequence(lb: ListBuffer[Int]): Unit
  def writeGene(listBuffer: ListBuffer[Int], elem: Int) : Unit = {
    if(rateRemove)
      return

    if(rateAppend)
      listBuffer += Genome.getRandomGene

    if(rateModify)
      listBuffer += (elem + (math.random * 6 - 3)).toInt
    else
      listBuffer += elem

    if(rateAppend)
      listBuffer += Genome.getRandomGene
  }

  def getMarker: Int
  def getId(): Int

  def isRemove = 0.01
  def isModify = 0.02
  def isAppend = 0.01

  def rateAppend:Boolean = math.random < isAppend
  def rateRemove:Boolean = math.random < isRemove
  def rateModify:Boolean = math.random < isModify

  def endBuild(genome: Genome) = {}
  def run = {}

  override def toString: String = this.getClass.toString + " id = " + getId()
}
