package info.omeg.genome.sensor

import info.omeg.genome.{Genome, IdGenerator}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 02.11.16.
  */
object SensorLength extends SensorLength(new IdGenerator)
{
  val marker = 9
  id = - marker
  def readSequence(idGenerator: IdGenerator, iter: Iterator[Int]): SensorLength = {
    var srcid:Int = 0
    if(iter.hasNext) srcid = iter.next()

    new SensorLength(idGenerator) {
      id = srcid
    }
  }
}

class SensorLength(idGenerator: IdGenerator) extends Sensor {
  var id = idGenerator.getId

  var currentLevel: Double = 0

  override def getCurrentLevel: Double = if (currentSnake != null) currentSnake.segments.length else 0

  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    lb += getId()
  }

  override def getMarker: Int = SensorLength.marker

  override def getId(): Int = id
}
