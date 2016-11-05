package info.omeg.genome.processor

import info.omeg.genome.sensor.{DummyEmitter, Emitter}
import info.omeg.genome.{GeneStrand, Genome, IdGenerator}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */

object Synapse extends Synapse(new IdGenerator, DummyEmitter)
{
  val marker: Int = 6
  id = -6
  def readSequence(idGenerator: IdGenerator, src: Iterator[Int]): Synapse = {
    val result = new Synapse(idGenerator, DummyEmitter)
    if(src.hasNext) {
      result.id = src.next()
    }

    if(src.hasNext) {
      result.weight = src.next().toDouble / weightMultiplier
    }

    if(src.hasNext) {
      sourceId = src.next()
    }

    result
  }
}

class Synapse(idGenerator: IdGenerator, src: Emitter) extends Emitter with SignalProcessor with GeneStrand {
  val weightMultiplier:Double = 10
  var weight:Double = math.random * weightMultiplier - weightMultiplier/2
  var source:Emitter = src
  var sourceId: Int = src.getId
  var id: Int = idGenerator.getId
  var currentLevel:Double = 0
  override def getCurrentLevel: Double = currentLevel

  def getLevel: Double = getWeight * source.getWeightedLevel


  override def run: Unit = {
    currentLevel = getLevel
  }

  override def getWeight: Double = weight

  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    writeGene(lb, getId)
    writeGene(lb, (weight * weightMultiplier).toInt)
    writeGene(lb, source.getId)
  }

  override def getMarker: Int = Synapse.marker

  override def getId(): Int = id

  override def toString: String = super.toString + ", source = " + sourceId + ", weight=" + getWeight

  override def endBuild(genome: Genome): Unit = {
    val v = genome.find(sourceId)
    v.foreach { x =>
      if(x.isInstanceOf[Emitter] && x != this) {
        source = x.asInstanceOf[Emitter]
        sourceId = source.getId()
      }
    }
  }

}
