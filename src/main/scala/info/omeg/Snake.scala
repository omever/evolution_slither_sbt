package info.omeg

import info.omeg.genome.sensor.Sensor
import info.omeg.genome.sink.Sink
import info.omeg.genome.{GeneStrand, Genome}

import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Shape}
import scalafx.stage.Stage

/**
  * Created by grigorii on 25.10.16.
  */
class Segment(xParam: Double, yParam: Double, angleParam: Double) {
  val segmentLength = 3
  var sX:Double = xParam
  var sY:Double = yParam
  var p:Double = angleParam
  var eX:Double = sX + math.cos(p) * segmentLength
  var eY:Double = sY + math.sin(p) * segmentLength

  def draw = {
  }

  override def toString: String = "Segment x=" + sX + ", y=" + sY + ", eX=" + eX + ", eY=" + eY
}

class Snake extends GeneStrand {
  def dropEgg: Egg = {
    if(eggs.length > 0) {
      val egg = eggs.head
      eggs = eggs.drop(1)
      egg
    } else {
      null
    }
  }

  def offScreen: Boolean = segments.head.sX < 0 || segments.head.sY < 0 || segments.head.sX > Genome.getWidth || segments.head.sY > Genome.getWidth

  var sensors = List.empty[Sensor]
  var sinks = List.empty[Sink]
  var snakeGenome: Genome = null

  override def endBuild(genome: Genome): Unit = {
    sensors = genome.source.flatMap({
      case v: Sensor => Some(v)
      case _ => None
    })
    sinks = genome.source.flatMap({
      case v: Sink => Some(v)
      case _ => None
    })
    sinks.foreach(v => v.snake = this)
    snakeGenome = genome
    Ui()
  }

  def learn(image: WritableImage) = {
    sensors.foreach(x => x.learn(image, segments.head.sX, segments.head.sY, this))
  }

  def think = {
    snakeGenome.source.foreach(f => f.run)
  }

  def updateUi(image: WritableImage) : Unit = {
    learn(image)
    think
    age -= 1
    if(age <= 0)
      dead = true
    if(dead)
      return
    val h = segments.head
    var lP = startAngle
    var nX = h.sX + math.cos(lP) * h.segmentLength
    var nY = h.sY + math.sin(lP) * h.segmentLength
    var nA = lP

    if(food != null)
      energy += food.tryConsume(nX.toInt, nY.toInt)

    segments.foreach(s => {
      s.eX = s.sX
      s.eY = s.sY
      s.sX = nX
      s.sY = nY
      nX = s.eX
      nY = s.eY
      lP = math.atan2(s.eY-s.sY, s.eX - s.sX)
    })

    if(energy > 5) {
      segments += new Segment(nX, nY, lP)
      energy -= 5
    }


//    if(snakeGenome.getGeneration > 0) {
      segments.foreach(s => {
        s.p = math.atan2(s.eY - s.sY, s.eX - s.sX)
        var x = s.sX
        var y = s.sY
        while (math.abs(x - s.eX) > 0.5 || math.abs(y - s.eY) > 0.5) {
          if (x.toInt >= 0 && x.toInt < Genome.getWidth && y.toInt > 0 && y.toInt < Genome.getWidth) {
            image.pixelWriter.setColor(x.toInt, y.toInt, Color.Black)
          }
          x = x + math.cos(s.p)
          y = y + math.sin(s.p)
        }
      })
  //  }
    //gc.restore()
  }

  def Ui() = {
    var elements = List.newBuilder[Line]
    var segmentBuilder = List.newBuilder[Segment]

    var lX:Double = getx
    var lY:Double = gety
    var lA = startAngle
    segments.clear()
    for( a <- 1 to length) {
      val segment = new Segment(lX, lY, lA)

      segments.+=(segment)

      lX = segment.eX
      lY = segment.eY
      lA = lA + turnRatio

      //println(segment)
    }
  }

  def notdead = !dead

  var dead = false
  var length = 4
  var segments = new MutableList[Segment]
  var startAngle = math.Pi * 2 * math.random
  var x: Double = (Genome.getWidth * math.random)
  var y: Double = (Genome.getWidth * math.random)
  var turnRatio:Double = 0.1
  var energy = 0
  var age = 5000

  def getx = x.toInt
  def gety = y.toInt

  override def toString: String = "Snake length=" + length + ", angle=" + startAngle + ", x=" + x + ", y=" + y

  def turnLeft() = {
    startAngle = startAngle - 0.1
//    println("turnLeft called")
  }

  def turnRight() = {
    startAngle = startAngle + 0.1
//    println("turnRight called")
  }

  var eggs = new MutableList[Egg]

  def layEgg = {
/*    if(energy > 3) {
      eggs += new Egg(segments.last.eX.toInt, segments.last.eY.toInt, snakeGenome.evolve)
      energy -= 3
    }*/

//    println("Lay egg called")
  }

  override def writeSequence(lb: ListBuffer[Int]): Unit = {

  }

  override def getMarker: Int = Snake.getMarker

  override def getId(): Int = -1

  var food: FoodDeposit = null
}

object Snake extends Snake {
  override def getMarker: Int = 1
}
