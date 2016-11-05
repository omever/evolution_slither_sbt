package info.omeg

import info.omeg.genome.Genome

import scala.collection.mutable
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color

/**
  * Created by grigorii on 01.11.16.
  */
class FoodDeposit {
  var baseDepositSize = Genome.depositSize

  def tryConsume(x: Int, y: Int): Int = {
    var value = 0
    foodDeposit = foodDeposit.filter(chunk => {
      if(math.abs(chunk.getX - x) < foodProximity && math.abs(chunk.getY - y) < foodProximity) {
        value += chunk.getEnergy
        //println("Thy consumed")
        false
      } else {
        true
      }
    })
    //println("Deposit size: " + foodDeposit.size)
    value
  }


  var depositSize = Genome.depositSize
  val foodProximity = 2
  var foodDeposit = new mutable.MutableList[FoodChunk]

  def restoreDeposit = {
    if(foodDeposit.length < depositSize) {
      for(a <- 1 to depositSize - foodDeposit.length) {
        foodDeposit += FoodChunk.random
      }
    }
  }

  def draw(writableImage: WritableImage) = {
    foodDeposit.foreach(f => {
      for(i <- -1 to 1)
        for(j <- -1 to 1)
          writePixel(writableImage, f.getX+i, f.getY+j, Color.Pink)
    })
  }

  def writePixel(writableImage: WritableImage, x: Int, y: Int, color: Color) = {
    if(x >= 0 && x < Genome.getWidth && y >= 0 && y < Genome.getWidth) {
      writableImage.pixelWriter.setColor(x, y, color)
    }
  }

}
