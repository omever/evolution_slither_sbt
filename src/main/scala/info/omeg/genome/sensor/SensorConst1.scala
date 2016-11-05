package info.omeg.genome.sensor

import info.omeg.genome.GeneStrand

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */

object SensorConst1 extends SensorConst1 {
  val marker: Int = 4
}

class SensorConst1 extends Sensor with GeneStrand {
  override def getCurrentLevel: Double = 1

  override def writeSequence(lb: ListBuffer[Int]): Unit = {}

  override def getMarker: Int = SensorConst1.marker

  override def getId(): Int = -4
}
