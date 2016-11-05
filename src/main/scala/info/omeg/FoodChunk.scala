package info.omeg

import info.omeg.genome.Genome

/**
  * Created by grigorii on 01.11.16.
  */
object FoodChunk {
  def random: FoodChunk = {
    new FoodChunk((math.random * Genome.getWidth).toInt, (math.random * Genome.getWidth).toInt, (math.random*5 + 1).toInt)
  }
}
class FoodChunk(x: Int, y: Int, energy: Int) {
  def getX: Int = x
  def getY: Int = y
  def getEnergy: Int = energy
}
