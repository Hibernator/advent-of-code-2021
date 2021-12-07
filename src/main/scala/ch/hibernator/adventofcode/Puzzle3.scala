package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Puzzle3 extends App:
  private val source = Source.fromFile("input/input3.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class NumBits(zeros: Int, ones: Int)

  val inputSize = input.size
  val inputBinary = input.map(_.toCharArray.map(_.toString.toInt).toSeq)

  val gammaBinary = (0 until inputBinary.head.length).map { bitPosition =>
    val sum = inputBinary.map(_(bitPosition)).sum
    if (sum > inputSize / 2) then 1 else 0
  }
  val epsilonBinary = gammaBinary.map(bit => if (bit == 0) then 1 else 0)

  def binaryToDecimal(binary: Seq[Int]): Int =
    binary
      .foldLeft((binary.length - 1, 0)) { case ((exponent, acc), nextBit) =>
        (
          exponent - 1,
          acc + nextBit * scala.math.pow(2, exponent).toInt
        )
      }
      ._2

  val gamma = binaryToDecimal(gammaBinary)
  val epsilon = binaryToDecimal(epsilonBinary)
  val result1 = gamma * epsilon
  println(result1)

  def mostCommonBitInPosition(numbers: Seq[Seq[Int]], bitPosition: Int): Int =
    val sum = numbers.map(_(bitPosition)).sum
    if (sum >= numbers.size.toDouble / 2) then 1 else 0

  def leastCommonBitInPosition(numbers: Seq[Seq[Int]], bitPosition: Int): Int =
    val sum = numbers.map(_(bitPosition)).sum
    if (sum >= numbers.size.toDouble / 2) then 0 else 1

  def isNumberWithBitInPosition(number: Seq[Int], bitPosition: Int, bit: Int) =
    number(bitPosition) == bit

  def filterNumbersByMostCommonBit(
      numbers: Seq[Seq[Int]],
      bitPosition: Int
  ): Seq[Int] =
    val mostCommonBit = mostCommonBitInPosition(numbers, bitPosition)
    val filteredNumbers =
      numbers.filter(isNumberWithBitInPosition(_, bitPosition, mostCommonBit))
    if (filteredNumbers.size == 1) then filteredNumbers.head
    else filterNumbersByMostCommonBit(filteredNumbers, bitPosition + 1)

  def filterNumbersByLeastCommonBit(
      numbers: Seq[Seq[Int]],
      bitPosition: Int
  ): Seq[Int] =
    val leastCommonBit = leastCommonBitInPosition(numbers, bitPosition)
    println(s"Least common bit is $leastCommonBit")
    val filteredNumbers =
      numbers.filter(isNumberWithBitInPosition(_, bitPosition, leastCommonBit))
    println(
      s"Filtered numbers are ${filteredNumbers.map(_.mkString("")).mkString(", ")}"
    )
    if (filteredNumbers.size == 1) then filteredNumbers.head
    else filterNumbersByLeastCommonBit(filteredNumbers, bitPosition + 1)

  val oxygenRating = binaryToDecimal(
    filterNumbersByMostCommonBit(inputBinary, 0)
  )
  println(s"Oxygen rating is $oxygenRating")

  val co2Rating = binaryToDecimal(filterNumbersByLeastCommonBit(inputBinary, 0))
  println(s"CO2 rating is $co2Rating")

  println(s"Life support rating is ${oxygenRating * co2Rating}")

end Puzzle3
