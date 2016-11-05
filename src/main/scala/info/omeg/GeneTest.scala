package info.omeg

import info.omeg.genome.Genome
import info.omeg.genome.processor.Synapse
import info.omeg.genome.sensor.SensorConst1
import info.omeg.genome.sink.{SinkTurnLeft, SinkTurnRight}

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 01.11.16.
  */
object GeneTest {
  def main(args: Array[String]): Unit = {
    println("Testing genes:")
    val lb = new ListBuffer[Int]
    for(i <- 1 to 50) {
      lb += (math.random * 20).toInt
    }
    var g = new Genome(lb.result())
/*     for(k <- 1 to 50) {
      println(g)
      g = g.evolve
    }
*/

    val sink = new SinkTurnLeft(g.idGenerator)
    val sink2 = new SinkTurnRight(g.idGenerator)

    val sensor = new SensorConst1
    val synapse = new Synapse(g.idGenerator, sensor)

    sink.synapses = List.apply[Synapse](synapse)
    synapse.run
    sink.run

    println(synapse)

    //synapse.weight = 3

    println(sink.toString + sink.getCurrentLevel)


    //println(g.evolve)
  }
}
