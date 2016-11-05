package info.omeg

import info.omeg.genome.Genome

import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color

/**
  * Created by grigorii on 02.11.16.
  */

object Egg {
  var timer = 15
}

class Egg(x: Int, y: Int, genome: Genome) {
  def draw(writeableImage: WritableImage): Unit = {
    for(i <- -1 to 1)
      for(j <- -1 to 1)
        writePixel(writeableImage, getX+i, getY+j, Color.Red)
  }


  def writePixel(writableImage: WritableImage, x: Int, y: Int, color: Color) = {
    if(x >= 0 && x < Genome.getWidth && y >= 0 && y < Genome.getWidth) {
      writableImage.pixelWriter.setColor(x, y, color)
    }
  }


  def getX = x
  def getY = y
  def getGenome = genome

  var timer = Egg.timer
}
