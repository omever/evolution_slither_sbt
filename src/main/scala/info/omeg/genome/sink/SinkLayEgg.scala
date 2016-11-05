package info.omeg.genome.sink

import info.omeg.Snake
import info.omeg.genome.{GeneStrand, Genome, IdGenerator}
import info.omeg.genome.processor.SynapseLinkage

/**
  * Created by grigorii on 02.11.16.
  */

object SinkLayEgg extends SynapseLinkage {
  val marker = 10

  def readSequence(idGenerator: IdGenerator, src: Iterator[Int]): GeneStrand = {
    val result = new SinkLayEgg(idGenerator)
    result.readSequence(src)
    result
  }
}

class SinkLayEgg(idGenerator: IdGenerator) extends Sink(idGenerator) {
  override def getMarker: Int = SinkLayEgg.marker

  override def getId(): Int = -9

  override var snake: Snake = null

  override def run: Unit = {
    super.run
    if(getWeightedLevel > getWeight && snake != null)
      snake.layEgg
  }
}
