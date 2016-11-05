package info.omeg.genome.sink

import info.omeg.genome.{GeneStrand, Genome, IdGenerator}
import info.omeg.Snake
import info.omeg.genome.processor.SynapseLinkage

/**
  * Created by grigorii on 01.11.16.
  */
object SinkTurnLeft extends SynapseLinkage {
  val marker = 7

  def readSequence(idGenerator: IdGenerator, src: Iterator[Int]): GeneStrand = {
    val result = new SinkTurnLeft(idGenerator)
    result.readSequence(src)
    result
  }
}

class SinkTurnLeft(idGenerator: IdGenerator) extends Sink(idGenerator) {
  override def getMarker: Int = SinkTurnLeft.marker

  override def getId(): Int = -7

  override var snake: Snake = null

  override def run: Unit = {
    super.run
    if(getCurrentLevel > getWeight && snake != null)
      snake.turnLeft
  }
}
