package info.omeg.genome.sensor

import info.omeg.genome.{GeneStrand, Genome, IdGenerator}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */
object SensorEye extends SensorEye(new IdGenerator, 0, 0) {
  id = -5
  def readSequence(idGenerator: IdGenerator, iter: Iterator[Int]): SensorEye = {
    var length = (math.random * 5).toInt
    var angle = math.random * 360
    var srcid:Int = 0
    if(iter.hasNext) srcid = iter.next()
    if(iter.hasNext) length = iter.next()
    if(iter.hasNext) {
      angle = math.toRadians(iter.next())
    }

    new SensorEye(idGenerator, length, angle) {
      id = srcid
    }
  }

  val marker: Int = 5
}

class SensorEye(idGenerator: IdGenerator, l: Int, a: Double) extends Sensor with GeneStrand {
  var length = l
  var angle = a
  var id = idGenerator.getId

  override def getCurrentLevel: Double = {
    if(currentSnake.segments.nonEmpty) {
      val snakeAngle: Double = currentSnake.segments.head.p
      val senseX = currentX + length * math.cos(snakeAngle + angle)
      val senseY = currentY + length * math.sin(snakeAngle + angle)
      if (senseX < 0 || senseY < 0 || senseX >= (Genome.getWidth - 1) || senseY >= Genome.getWidth - 1)
        1
      else
        environment.pixelReader.get.getColor(senseX.toInt, senseY.toInt).brightness
    } else 0
  }

  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    lb += getId()
    lb += length
    lb += math.toDegrees(angle).toInt
  }

  override def getMarker: Int = SensorEye.marker

  override def getId(): Int = id
}
