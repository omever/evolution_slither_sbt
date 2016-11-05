package info.omeg.genome.sensor

import info.omeg.genome.GeneStrand

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */

object DummyEmitter extends Emitter {
  override def getCurrentLevel: Double = math.random

  override def writeSequence(lb: ListBuffer[Int]): Unit = {

  }

  override def getMarker: Int = -2

  override def getId(): Int = -2

  override def getWeight: Double = 1
}

abstract class Emitter extends GeneStrand{
  def getCurrentLevel: Double
  def getWeight: Double
  def getWeightedLevel: Double = getCurrentLevel*getWeight //1/(1+math.exp(getCurrentLevel*getWeight))
}
