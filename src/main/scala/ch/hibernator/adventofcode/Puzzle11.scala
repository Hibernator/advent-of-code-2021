package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source

object Puzzle11 extends App:
  private val source = Source.fromFile("input/input11.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Coordinates(row: Int, column: Int)
  case class Octopus(coordinates: Coordinates, energy: Int, flashed: Boolean = false) {

    def increaseEnergy: Octopus =
      val withIncreasedEnergy = this.copy(energy = this.energy + 1)
      if withIncreasedEnergy.energy > 9 then withIncreasedEnergy.copy(energy = 0, flashed = true)
      else withIncreasedEnergy
  }

  val initialEnergies = input.map { line =>
    line.map(_.toString.toInt)
  }

  val state: mutable.Map[Coordinates, Octopus] = mutable.Map()

  def initializeState(): Unit =
    for {
      row <- input.indices
      col <- input.indices
    } {
      val coordinates = Coordinates(row, col)
      state.put(coordinates, Octopus(coordinates, initialEnergies(row)(col)))
    }

  initializeState()

  def oneStep: Int =

    def getAdjacent(coordinates: Coordinates): Seq[Octopus] =
      Seq(
        state.get(coordinates.copy(coordinates.row - 1, coordinates.column - 1)),
        state.get(coordinates.copy(coordinates.row - 1, coordinates.column)),
        state.get(coordinates.copy(coordinates.row - 1, coordinates.column + 1)),
        state.get(coordinates.copy(coordinates.row, coordinates.column - 1)),
        state.get(coordinates.copy(coordinates.row, coordinates.column + 1)),
        state.get(coordinates.copy(coordinates.row + 1, coordinates.column - 1)),
        state.get(coordinates.copy(coordinates.row + 1, coordinates.column)),
        state.get(coordinates.copy(coordinates.row + 1, coordinates.column + 1))
      ).flatten

    def resetFlashed(): Unit = state.foreach { case (coordinates, octopus) =>
      state.put(coordinates, octopus.copy(flashed = false))
    }

    var newFlashes = 0

    def processOctopus(octopus: Octopus): Unit = {
      if !octopus.flashed then
        val octopusWithIncreasedEnergy = octopus.increaseEnergy
        state.put(octopus.coordinates, octopusWithIncreasedEnergy)

        if octopusWithIncreasedEnergy.flashed then
          newFlashes = newFlashes + 1
          val adjacentCoordinates = getAdjacent(octopus.coordinates).filterNot(_.flashed).map(_.coordinates)
          adjacentCoordinates.foreach(coordinates => processOctopus(state(coordinates)))

    }

    for {
      row <- input.indices
      col <- input.indices
    } processOctopus(state(Coordinates(row, col)))

    resetFlashed()
    newFlashes

  val numFlashes = (1 to 100).foldLeft(0) { case (accFlashes, _) =>
    accFlashes + oneStep
  }

  println(numFlashes)

  // part 2 starts here
  initializeState()

  @tailrec
  def nextStep(previousStep: Int): Int = {
    val flashes = oneStep
    if flashes == 100 then previousStep + 1 else nextStep(previousStep + 1)
  }

  val stepWithAllFlashing = nextStep(0)

  println(stepWithAllFlashing)

end Puzzle11
