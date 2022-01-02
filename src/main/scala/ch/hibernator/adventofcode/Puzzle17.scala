package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle17 extends App:
  private val source = Source.fromFile("input/input17.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val targetTopLeft = (20, -5)
  val targetBottomRight = (30, -10)

  case class Coordinates(x: Int, y: Int) {
    def moveByVector(velocityVector: VelocityVector): Coordinates =
      copy(x = x + velocityVector.x, y = y + velocityVector.y)
  }

  case class VelocityVector(x: Int, y: Int) {
    def evolve: VelocityVector =
      val newX = if x > 0 then x - 1 else if x < 0 then x + 1 else 0
      VelocityVector(newX, y - 1)
  }

  enum MissedReason:
    case TooDeep, TooSlow, TooFast

  /** Calculates a trajectory to the target area.
    * @param initialVelocity
    *   initial velocity
    * @return
    *   trajectory with the last stop in the target area or set of reasons it missed
    */
  def trajectory(initialVelocity: VelocityVector): Either[Set[MissedReason], Seq[Coordinates]] =
    @tailrec
    def nextStep(
        trajectoryAcc: Seq[Coordinates],
        velocity: VelocityVector
    ): Either[Set[MissedReason], Seq[Coordinates]] =
      println(trajectoryAcc)
      val last = trajectoryAcc.last

      val insideTarget: Boolean =
        last.x >= targetTopLeft._1 && last.x <= targetBottomRight._1 && last.y <= targetTopLeft._2 &&
          last.y >= targetBottomRight._2

      val tooDeep: Option[MissedReason] =
        Option.when(last.y < targetBottomRight._2 && velocity.y < 1)(MissedReason.TooDeep)
      val tooSlow: Option[MissedReason] = Option.when(last.x < targetTopLeft._1 && velocity.x < 1)(MissedReason.TooSlow)
      val tooFast: Option[MissedReason] = Option.when(last.x > targetBottomRight._1)(MissedReason.TooFast)

      def missed: Boolean = tooDeep.isDefined || tooSlow.isDefined || tooFast.isDefined

      if insideTarget then Right(trajectoryAcc)
      else if missed then Left(Set(tooDeep, tooSlow, tooFast).flatten)
      else nextStep(trajectoryAcc :+ last.moveByVector(velocity), velocity.evolve)

    nextStep(Seq(Coordinates(0, 0)), initialVelocity)

  val lowestXSpeed: Int =
    val initialGuess = Math.sqrt(targetTopLeft._1 * 2).toInt

    def createXPositions(initialSpeed: Int): Seq[Int] =
      (1 to initialSpeed).reverse.foldLeft(Seq[Int](0)) { case (xs, increase) =>
        xs :+ xs.last + increase
      }

    def isTooLow(guess: Int): Boolean =
      createXPositions(guess).forall(_ < targetTopLeft._1)

    0

  val path = trajectory(VelocityVector(6, 9))
  val highestPosition = path.map(_.map(_.y).max)
  println(path)
  println(highestPosition)

end Puzzle17
