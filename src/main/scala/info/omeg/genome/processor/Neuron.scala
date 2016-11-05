package info.omeg.genome.processor

import info.omeg.genome._
import info.omeg.genome.sensor.Emitter

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 31.10.16.
  */
object Neuron extends Neuron(new IdGenerator) with SynapseLinkage {
  val marker: Int = 2
  val weightMultiplier:Double = 10
  id = -2
  def readSequence(idGenerator: IdGenerator, src: Iterator[Int]): GeneStrand = {
    val result = new Neuron(idGenerator)
    val lb = new ListBuffer[Int]

    if(src.hasNext) {
      result.id = src.next()
    }

    if(src.hasNext) {
      result.weight = src.next().toDouble / weightMultiplier
    }

    result.synapseIds = readSynapseIds(src).result()

    result
  }
}

class Neuron(idGenerator: IdGenerator) extends Emitter with SignalProcessor with GeneStrand {
  var synapses: List[Synapse] = List.empty[Synapse]
  var synapseIds = List.empty[Int]
  var weight: Double = math.random * 2 - 1
  var id: Int = idGenerator.getId

  override def getWeight: Double = weight

  override def run: Unit = {
    super.run
    currentLevel = synapses.foldRight[Double](0)((w, h) => h + w.getWeightedLevel)
  }

  override def getWeightedLevel: Double = if (getCurrentLevel >= getWeight) 1 else if (-getCurrentLevel < -getWeight) -1 else 0

  var currentLevel: Double = 0

  override def getCurrentLevel: Double = currentLevel

  override def writeSequence(lb: ListBuffer[Int]): Unit = {
    writeGene(lb, getId)
    writeGene(lb, (weight * Neuron.weightMultiplier).toInt)
    synapses.foreach(x => writeGene(lb, x.getId))
    writeGene(lb, 0)
  }

  override def getMarker: Int = Neuron.marker

  override def toString: String = super.toString + ", level="+getCurrentLevel+", weight=" + getWeight + ", synapses[" + synapses.foldLeft("")({(x, y) => y + x + " + "}) + "]"

  override def endBuild(genome: Genome): Unit = {
    synapses = synapseIds.flatMap(id => {
      val xs = genome.find(id)
      if (xs.isEmpty) {
        None
      } else {
        val v = genome.find(id).head
        v match {
          case x: Synapse => Some(x)
          case _ => None
        }
      }
    })
  }

  override def getId(): Int = id
}
