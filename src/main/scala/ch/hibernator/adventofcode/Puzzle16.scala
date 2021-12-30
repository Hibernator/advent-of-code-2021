package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle16 extends App:
  private val source = Source.fromFile("input/input16.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val hexMessage = input.head

  val hexToBinaryMap: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  def hexToBinary(hex: String): String =
    val builder = new StringBuilder(hex.length * 4, "")
    hex.foreach(hexSign => builder.append(hexToBinaryMap(hexSign)))
    builder.toString()

  def binaryToDecimal(binary: String): Long =
    binary.reverse.zipWithIndex.foldLeft(0L) { case (acc, (currentNum, exponent)) =>
      if currentNum == '0' then acc else acc + Math.pow(2, exponent).longValue()
    }

  def isLiteralPacket(rawBinaryPacket: String): Boolean = rawBinaryPacket.substring(3, 6).pipe(binaryToDecimal) == 4L

  trait Packet {
    def length: Int
    def version: Long
    def packetType: Long
    def subPackets: Seq[Packet]
    def sumOfVersions: Long = version + subPackets.map(_.sumOfVersions).sum
    def evaluate: Long
  }

  object Packet {
    def parse(rawBinaryPacket: String): Packet =
      if isLiteralPacket(rawBinaryPacket) then LiteralPacket.parse(rawBinaryPacket)
      else OperatorPacket.parse(rawBinaryPacket)

  }

  case class LiteralPacket(version: Long, value: Long, length: Int) extends Packet {
    val packetType = 4L
    val subPackets: Seq[Packet] = Nil
    val evaluate: Long = value
  }

  object LiteralPacket {
    def parse(packetRaw: String): LiteralPacket =
      def parseValue(rawValue: String): (Long, Int) =
        @tailrec
        def parseNextGroup(acc: String, numGroups: Int, remainingString: String): (String, Int) =
          val nextGroupBits = remainingString.tail.take(4)
          if remainingString.head == '0' then (acc ++ nextGroupBits, numGroups + 1)
          else parseNextGroup(acc ++ nextGroupBits, numGroups + 1, remainingString.drop(5))

        val (binaryValue, numGroups) = parseNextGroup("", 0, rawValue)
        (binaryToDecimal(binaryValue), numGroups * 5)

      val (value, length) = parseValue(packetRaw.drop(6))
      LiteralPacket(packetRaw.take(3).pipe(binaryToDecimal), value, 6 + length)
  }

  case class OperatorPacket(version: Long, packetType: Long, subPackets: Seq[Packet], lengthType: Int) extends Packet {
    val length: Int = 7 + (if lengthType == 0 then 15 else 11) + subPackets.map(_.length).sum
    val evaluate: Long =
      packetType match {
        case 0L => subPackets.map(_.evaluate).sum
        case 1L => subPackets.map(_.evaluate).product
        case 2L => subPackets.map(_.evaluate).min
        case 3L => subPackets.map(_.evaluate).max
        case 5L => if subPackets.head.evaluate > subPackets(1).evaluate then 1 else 0
        case 6L => if subPackets.head.evaluate < subPackets(1).evaluate then 1 else 0
        case 7L => if subPackets.head.evaluate == subPackets(1).evaluate then 1 else 0
      }
  }

  object OperatorPacket {
    def parse(packetRaw: String): OperatorPacket =
      def parseByLengthInBits(rawSubPackets: String): Seq[Packet] =
        @tailrec
        def parseNextPacket(accPackets: Seq[Packet], remainingRaw: String): Seq[Packet] =
          if remainingRaw.isEmpty then accPackets
          else
            val parsedPacket = Packet.parse(remainingRaw)
            parseNextPacket(accPackets :+ parsedPacket, remainingRaw.drop(parsedPacket.length))

        parseNextPacket(Seq[Packet](), rawSubPackets)

      def parsebyNumberOfSubPackets(rawSubPackets: String, numPackets: Long): Seq[Packet] =
        @tailrec
        def parseNextPacket(accPackets: Seq[Packet], remainingRaw: String): Seq[Packet] =
          if accPackets.size == numPackets then accPackets
          else
            val parsedPacket = Packet.parse(remainingRaw)
            parseNextPacket(accPackets :+ parsedPacket, remainingRaw.drop(parsedPacket.length))

        parseNextPacket(Seq[Packet](), rawSubPackets)

      val lengthType = if packetRaw(6) == '0' then 0 else 1
      val subPackets = if lengthType == 0 then
        val length = packetRaw.substring(7, 22).pipe(binaryToDecimal)
        parseByLengthInBits(packetRaw.substring(22, 22 + length.intValue()))
      else
        val numOfSubPackets = packetRaw.substring(7, 18).pipe(binaryToDecimal)
        parsebyNumberOfSubPackets(packetRaw.drop(18), numOfSubPackets)

      OperatorPacket(
        packetRaw.take(3).pipe(binaryToDecimal),
        packetRaw.substring(3, 6).pipe(binaryToDecimal),
        subPackets,
        lengthType
      )
  }

  val packet = Packet.parse(hexToBinary(hexMessage))
  val result = packet.sumOfVersions

  println(result)

  val result2 = packet.evaluate
  println(result2)

end Puzzle16
