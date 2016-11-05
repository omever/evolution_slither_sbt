package info.omeg.genome.sink

import info.omeg.Snake
import info.omeg.genome.{GeneStrand, Genome, IdGenerator}
import info.omeg.genome.processor.{Neuron, SignalProcessor, SynapseLinkage}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 01.11.16.
  */
abstract class Sink(idGenerator: IdGenerator) extends Neuron(idGenerator) with SignalProcessor with SynapseLinkage {
  var snake : Snake

  override def getWeight: Double = weight

  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    writeGene(lb, (getWeight*10).toInt)
    synapses.foreach(x => writeGene(lb, x.getId))
    writeGene(lb, 0)
  }

  def readSequence(src: Iterator[Int]): Unit = {
    if(src.hasNext) weight = src.next().toDouble / 10
    if(weight < 0.9) {
      weight = 0.9
    }
    synapseIds = readSynapseIds(src).result()
  }

  override def getWeightedLevel: Double = {
    if(synapses.nonEmpty)
      super.getWeightedLevel
    else
      0
  }
}
