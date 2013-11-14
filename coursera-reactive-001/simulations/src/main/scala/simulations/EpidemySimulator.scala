package simulations

import math.random
import scala.util.Random.shuffle
class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val moveDelay: Int = 5
    val prevalenceRate = 1
    val transmissibilityRate = 0.4
    val prevalenceNumb = population * prevalenceRate / 100
    val dieProbability = 0.25
    val infectedPeriod = 6
    val sickPeriod = 14
    val immuneDeley = 16
    val healthyDeley = 18
    // to complete: additional parameters of simulation
  }

  import SimConfig._

  type Room1 = (Int, Int)
  val rows = roomRows +: (for (i <- 0 until roomRows) yield (i)) :+ 1
  val columns = roomColumns +: (for (i <- 0 until roomColumns) yield (i)) :+ 1

  val persons: List[Person] = (for (i <- 1 to 300) yield (new Person(i))).toList

  def infect(p: Person) = {
    p.infected = true

    p
  }
  def sickAction(p: Person) = {
    afterDelay(infectedPeriod) {
      p.sick = true
    }
    p
  }
  def dieAction(p: Person) = {
    afterDelay(sickPeriod) {
      if (shouldDie) p.dead = true
    }
    p
  }
  def immuneAction(p: Person) = {
    afterDelay(immuneDeley) {
      if (!p.dead) {
        p.sick = false
        p.immune = true
      }
    }
    p
  }
  def healthyAction(p: Person) = {
    afterDelay(healthyDeley) {
      if (!p.dead) {
        p.infected = false
        p.sick = false
        p.immune = false
      }

    }
    p
  }

  def neighbouringRooms(row: Int, col: Int) =
    List((row, columns(col + 2)), (row, columns(col)), (rows(row + 2), col), (rows(row), col))

  def badRoom(row: Int, col: Int) = !persons.filter(p =>
    p.row == row && p.col == col && (p.sick || p.dead || p.infected)).isEmpty

  def unluckyGuy = random <= transmissibilityRate
  def shouldDie = random <= dieProbability

  def applyRules(p: Person) =
    if (!p.infected && !p.immune && badRoom(p.row, p.col) && unluckyGuy)
      actions(p)

  def moveToNeighbouringRoom(p: Person): Unit = {

    def notInfected(room: Room1) = {
      val (row, col) = room
      val result = persons filter (p => p.row == row && p.col == col && !p.sick && !p.dead)
      !result.isEmpty
    }

    def move() = {
      afterDelay(moveDelay) {
        if (!p.dead) {
          val rooms = neighbouringRooms(p.row, p.col) filter notInfected
          shuffle(rooms) match {
            case (row, col) :: rest =>
              p.row = row
              p.col = col
              applyRules(p)
            case _ =>
          }
          moveToNeighbouringRoom(p)
        }
      }
    }

    if (!p.dead) move()

  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    //
    // to complete with simulation logic
    //

  }

  val actions = infect _ andThen sickAction _ andThen dieAction _ andThen immuneAction _ andThen healthyAction

  persons take prevalenceNumb map actions 
    

  persons map moveToNeighbouringRoom
}
