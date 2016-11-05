package info.omeg

import info.omeg.genome.Genome

import scala.collection.mutable
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.{Group, Scene, SnapshotParameters}
import scalafx.scene.image.WritableImage
import scalafx.scene.input.{KeyEvent, MouseButton, MouseEvent, ScrollEvent}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scala.util.control.Breaks._
import scalafx.scene.effect.{DropShadow, Shadow}
import scalafx.scene.shape.Line
/**
  * Created by grigorii on 25.10.16.
  */

abstract class ScrollEventHandler
{
  def execute(scrollEvent: ScrollEvent)
}

object MainUI extends JFXApp {
  val canvas = new Canvas(1000, 1000)
  val graphicsContext = canvas.graphicsContext2D
  var image = new WritableImage(Genome.getWidth, Genome.getWidth)
  val foodDeposit = new FoodDeposit
  var randomSnakeCounter : BigInt = BigInt.apply(0)
  var bornSnakeCounter : BigInt = BigInt.apply(0)
  canvas.translateX = 0
  canvas.translateY = 0

  def addASnake = {
    val randomGenome = Genome.random
    val snake = new Snake {
      food = foodDeposit
    }

    snake.endBuild(randomGenome)
    snake
  }

  def addASnake(genome: Genome) = {
    val snake = new Snake {
      food = foodDeposit
    }

    snake.endBuild(genome)
    snake
  }

  def addASnake(atX: Int, atY: Int) = {
    val randomGenome = Genome.random
    val snake = new Snake {
      food = foodDeposit
      x = atX
      y = atY
    }

    snake.endBuild(randomGenome)
    snake
  }

  var foodEvent = new ScrollEventHandler {
    override def execute(scrollEvent: ScrollEvent): Unit = {
      foodDeposit.baseDepositSize += (scrollEvent.deltaY).toInt
      if(foodDeposit.baseDepositSize < 0 ) foodDeposit.baseDepositSize = 0
      println("Changed deposit to :" + foodDeposit.baseDepositSize)
    }
  }

  var snakesEvent = new ScrollEventHandler {
    override def execute(scrollEvent: ScrollEvent): Unit = {
      Genome.maintainSnakesNum += (scrollEvent.deltaY/10).toInt
      if(Genome.maintainSnakesNum < 0 ) Genome.maintainSnakesNum = 0
      println("Changed maintain snakes to :" + Genome.maintainSnakesNum)
    }
  }

  var delayEvent = new ScrollEventHandler {
    override def execute(scrollEvent: ScrollEvent): Unit = {
      sleep += (scrollEvent.deltaY/10).toInt
      if(sleep < 0 ) sleep = 0
      if(sleep > 1000) sleep = 1000
      println("Changed runner sleep to :" + sleep)
    }
  }

  var eggTimerEvent = new ScrollEventHandler {
    override def execute(scrollEvent: ScrollEvent): Unit = {
      Egg.timer += (scrollEvent.deltaY/10).toInt
      if(Egg.timer < 0 ) Egg.timer = 0
      if(Egg.timer > 1000) Egg.timer = 1000
      println("Changed egg timer to :" + Egg.timer)
    }
  }

  var sleep = 0
  var sf = delayEvent

  stage = new JFXApp.PrimaryStage {
    var snakesListBuilder = mutable.MutableList.newBuilder[Snake]
    var infoBlock = new Group()

    val snakeCounter = new Label()
    infoBlock.children.addAll(snakeCounter)

    title.value = "Evolution of Slithering"
    width = 1000
    height = 1000
    scene = new Scene {
      fill = Color.Gray
      content = canvas
      handleEvent(ScrollEvent.Scroll) {
        se: ScrollEvent => sf.execute(se)
      }
      handleEvent(KeyEvent.KeyTyped) {
        ke: KeyEvent => {
          ke.character match {
            case "1" => sf = delayEvent
            case "2" => sf = foodEvent
            case "3" => sf = snakesEvent
            case "4" => sf = eggTimerEvent
          }
        }
      }
    }
    graphicsContext.drawImage(image, 0, 0)
    for(a <- 1 to Genome.maintainSnakesNum) {
      snakesListBuilder += addASnake
    }

    var snakes = snakesListBuilder.result
    var newSnakes = snakes
    var eggs = new mutable.MutableList[Egg]
    val snapshotParameters = new SnapshotParameters()
    snapshotParameters.fill = Color.Transparent

    var maximumGenomeLife = 0

    val thr = new Thread {


      override def run = {
        while (true) {
          Platform.runLater {
            var y = 0
            def getY = {y = y+10; y}
            graphicsContext.clearRect(0, 0, 1000, 1000)
            graphicsContext.drawImage(image, 0, 0)
            graphicsContext.save()
            graphicsContext.fill = Color.Red
            graphicsContext.fillText("Delay: " + sleep, 10, getY)
            graphicsContext.fillText("Snakes: " + snakes.length + " / " + newSnakes.length, 10, getY)
            graphicsContext.fillText("Genome Life: " + maximumGenomeLife, 10, getY)
            graphicsContext.fillText("Food deposit: " + foodDeposit.depositSize, 10, getY)
            graphicsContext.fillText("Snake stats: ", 10, getY)
            graphicsContext.fillText("           - random snakes " + randomSnakeCounter, 10, getY)
            graphicsContext.fillText("           - born   snakes " + bornSnakeCounter, 10, getY)
            graphicsContext.fill = Color.DarkGreen
            graphicsContext.fillText("To scroll delay     : press 1", 10, getY)
            graphicsContext.fillText("To scroll food      : press 2", 10, getY)
            graphicsContext.fillText("To scroll snakes    : press 3", 10, getY)
            graphicsContext.fillText("To scroll egg timer : press 4", 10, getY)
          }
          Thread.sleep(100)
        }
      }
    }
    thr.setDaemon(true)
    thr.start()

    def intersection(ax: Double, ay: Double, bx: Double, by: Double)
                    (cx: Double, cy: Double, dx: Double, dy: Double) : Boolean = {
      var d = (bx-ax)*(dy-cy) - (by-ay)*(dx-cx)
      if( d != 0 ) {
        var detu = (cx - ax)*(dy - cy) - (cy - ay)*(dx - cx)
        var detv = (cx - ax)*(by - ay) - (cy - ay)*(bx - ax)
        if(d < 0) {
          d = -d
          detu = -detu
          detv = -detv
        }

        if(detu >= 0 && detu <= d && detv >= 0 && detv <= d)
          true
        else false
      }
      else false
    }

    val thrCalc = new Thread {


      override def run = {
        while (true) {
          //println("Thread execution")
          newSnakes = snakes.clone()
          while (newSnakes.nonEmpty) {
            val writableImage = new WritableImage(Genome.getWidth, Genome.getWidth)
            var newMaximumGenomeLife = 0
            for (snake <- newSnakes) {
              newMaximumGenomeLife = math.max(snake.snakeGenome.getGeneration, newMaximumGenomeLife)

              if (snake.dead || snake.segments.isEmpty || snake.offScreen) {
                newSnakes = newSnakes.filterNot(x => x == snake)
              } else {
                val firstSegment = snake.segments.head
                def iLocal = intersection(firstSegment.sX, firstSegment.sY, firstSegment.eX, firstSegment.eY) _
                val it = newSnakes.iterator
                while (snake.notdead && it.hasNext) {
                  val i = it.next
                  if (i != snake && i.segments.nonEmpty) {
                    i.segments.foreach(s => {
                      if (iLocal(s.sX, s.sY, s.eX, s.eY)) {
                        snake.dead = true
                      }
                    })
                  }

                }

                if (!snake.dead) {
                  snake.learn(image)
                  snake.updateUi(writableImage)
/*                  val egg = snake.dropEgg
                  if (egg != null) {
                    eggs += egg
                  }*/
                }
              }
            }
            maximumGenomeLife = newMaximumGenomeLife
            foodDeposit.draw(writableImage)
            /*val ready = eggs.filter({ x => x.timer == 0 })
            ready.foreach({ x => {
              bornSnakeCounter = bornSnakeCounter + 1
              val s = addASnake(x.getX, x.getY)
              s.endBuild(x.getGenome)
              newSnakes += s
            }
            })
            eggs = eggs.filterNot({ x => x.timer == 0 })

            eggs.foreach(x => {
              x.draw(writableImage)
              x.timer -= 1
            })
            */
            image = writableImage
            /*
            if (newSnakes.length < Genome.maintainSnakesNum) {
              for (i <- 1 to (Genome.maintainSnakesNum - newSnakes.length))
                newSnakes += addASnake
              randomSnakeCounter = randomSnakeCounter + 1
            }
            snakes = newSnakes
            */
            if (newSnakes.length > Genome.maintainSnakesNum) {
              foodDeposit.depositSize = math.max(0, foodDeposit.baseDepositSize - (newSnakes.length - Genome.maintainSnakesNum))
            } else {
              foodDeposit.depositSize = foodDeposit.baseDepositSize
            }
            foodDeposit.restoreDeposit

            if (sleep > 0) {
              Thread.sleep(sleep)
            }
          }

          val k = snakes.sortWith((a, b) => a.segments.length > b.segments.length)
          val (bestSnakes, _) = k.splitAt(k.length / 2)
          snakes.clear()
          for(s <- bestSnakes) {
            if(snakes.length < Genome.maintainSnakesNum) {
              snakes += addASnake(s.snakeGenome.evolve)
              snakes += addASnake(s.snakeGenome.evolve)
            }
          }

          for(i <- 1 to (Genome.maintainSnakesNum - snakes.length))
            addASnake
        }
      }
    }

    thrCalc.setDaemon(true)
    thrCalc.start()
  }
}
