package info.omeg.genome.processor

import scala.collection.mutable.ListBuffer

/**
  * Created by grigorii on 02.11.16.
  */
trait SynapseLinkage {
  def readSynapseIds(src: Iterator[Int]): ListBuffer[Int] = {
    val lb = new ListBuffer[Int]

    var t: Int = -1
    while (src.hasNext && t != 0) {
      t = src.next()
      if(t != 0) lb += t
    }
    lb
  }
}
