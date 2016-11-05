package info.omeg.genome.sink

import info.omeg.genome.{GeneStrand, Genome, IdGenerator}
import info.omeg.Snake
import info.omeg.genome.processor.SynapseLinkage

/**
  * Created by grigorii on 01.11.16.
  */

object SinkTurnRight extends SynapseLinkage {
  val marker = 8

  def readSequence(idGenerator: IdGenerator, src: Iterator[Int]): GeneStrand = {
    val result = new SinkTurnRight(idGenerator)
    result.readSequence(src)
    result
  }
}

class SinkTurnRight(idGenerator: IdGenerator) extends Sink(idGenerator) {
  override def getMarker: Int = SinkTurnRight.marker

  override def getId(): Int = -8

  override var snake: Snake = null


  override def run: Unit = {
    super.run
    if(getWeightedLevel > getWeight && snake != null)
      snake.turnRight
  }

}
