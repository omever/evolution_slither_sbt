package info.omeg.genome

import info.omeg._
import info.omeg.genome.processor.{Neuron, Synapse}
import info.omeg.genome.sensor.{SensorConst0, SensorConst1, SensorEye, SensorLength}
import info.omeg.genome.sink.{SinkLayEgg, SinkTurnLeft, SinkTurnRight}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 01.11.16.
  */
object Genome {
  var maintainSnakesNum: Int = 200

  val depositSize = 1500
  def getRandomGene : Int = (math.random * 20).toChar
  def getWidth = 1000
  def random : Genome = {
    val lb = new ListBuffer[Int]
    for (i <- 1 to 200) {
      lb += getRandomGene
    }
    new Genome(lb.result())
  }
}
class IdGenerator
{
  var id : Int = 0
  def getId: Int = {
    id = id + 1
    id
  }
}

class Genome(src: List[Int], generation: Int = 0) {
  val idGenerator = new IdGenerator

  def find(id: Int): List[GeneStrand] = source.filter(p => p.getId.equals(id))
  def getGeneration = generation
  val source: List[GeneStrand] = {
    val lb = new ListBuffer[GeneStrand]

    val iter = src.iterator
    while(iter.hasNext) {
      val v = iter.next()
      v match {
        case SensorConst0.marker => lb += new SensorConst0
        case SensorConst1.marker => lb += new SensorConst1
        case Neuron.marker => lb += Neuron.readSequence(idGenerator, iter)
        case SensorEye.marker => lb += SensorEye.readSequence(idGenerator, iter)
        case Synapse.marker => lb += Synapse.readSequence(idGenerator, iter)
        case SensorLength.marker => lb += SensorLength.readSequence(idGenerator, iter)
        case SinkTurnLeft.marker => lb += SinkTurnLeft.readSequence(idGenerator, iter)
        case SinkTurnRight.marker => lb += SinkTurnRight.readSequence(idGenerator, iter)
        case SinkLayEgg.marker => lb += SinkLayEgg.readSequence(idGenerator, iter)
//        case Snake.getMarker => lb += Snake.readSequence(iter)
        case _ => lb += new BogusStrand(v)
      }
    }
    lb.result()
  }

  source.foreach(x => x.endBuild(this))

  def evolve: Genome = {
    val lb = new ListBuffer[Int]
    source.foreach(x => {
      if(!x.isInstanceOf[BogusStrand]) lb += x.getMarker
      x.writeSequence(lb)
    })
    new Genome(lb.result(), generation + 1)
  }


  override def toString: String = "Genome size=" + source.size + ", source = [" + source.foldLeft("")((v, g) => v + "\n\t" + g.toString) + "\n\t]"
}
