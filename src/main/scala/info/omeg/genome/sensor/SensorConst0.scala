package info.omeg.genome.sensor

import info.omeg.genome.GeneStrand

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */
object SensorConst0 extends SensorConst0 {
  val marker: Int = 3
}

class SensorConst0 extends Sensor with GeneStrand {
  override def getCurrentLevel: Double = 0

  override def writeSequence(lb: ListBuffer[Int]): Unit = {}

  override def getMarker: Int = SensorConst0.marker

  val id: Int = -3

  override def getId(): Int = id
}
