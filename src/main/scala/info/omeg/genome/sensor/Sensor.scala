package info.omeg.genome.sensor

import info.omeg.Snake

import scalafx.scene.image.WritableImage

/**
  * Created by grigorii on 31.10.16.
  */
abstract class Sensor extends Emitter{
  override def getCurrentLevel: Double
  var environment: WritableImage = null
  var currentX: Int = 0
  var currentY: Int = 0
  var currentSnake: Snake = null

  def learn(wi: WritableImage, x: Double, y: Double, snake: Snake): Unit = {
    environment = wi
    currentX = x.toInt
    currentY = y.toInt
    currentSnake = snake
  }

  def getWeight = 1
}
