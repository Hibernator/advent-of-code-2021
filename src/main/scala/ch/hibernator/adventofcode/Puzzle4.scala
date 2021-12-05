package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Puzzle4 extends App:
  private val source = Source.fromFile("input4.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Tile(number: Int, marked: Boolean = false)

  class Board(val rows: Seq[Seq[Tile]]):
    lazy val columns: Seq[Seq[Tile]] = (0 to 4).map { position =>
      rows.map(_(position))
    }

    def markNumber(num: Int): Board =
      val updatedRows = rows.map { row =>
        val maybeNum = row.find(_.number == num)
        maybeNum match {
          case None => row
          case Some(tile) =>
            row.updated(row.indexOf(tile), tile.copy(marked = true))
        }
      }
      new Board(updatedRows)

    def isAnyRowComplete: Boolean = rows.exists(_.forall(_.marked))
    def isAnyColumnComplete: Boolean = columns.exists(_.forall(_.marked))
    def isAnyRowOrColumnComplete: Boolean =
      isAnyRowComplete || isAnyColumnComplete
    def unmarkedNumbers: Seq[Int] =
      rows.flatten.filterNot(_.marked).map(_.number)

  end Board

  val drawnNumbers: Seq[Int] = input.head.split(",").map(_.toInt).toSeq
  val boards = input.tail
    .grouped(6)
    .map(_.tail)
    .toSeq
    .map { boardRaw =>
      boardRaw.map { rowRaw =>
        rowRaw
          .split(" ")
          .filterNot(_ == "")
          .map(_.toInt)
          .toSeq
          .map(Tile(_, false))
      }
    }
    .map(Board(_))

  @tailrec
  def findWinningBoardAndNumber(
      numPosition: Int,
      boards: Seq[Board]
  ): (Board, Int) =
    val drawnNumber = drawnNumbers(numPosition)
    val updatedBoards = boards.map(_.markNumber(drawnNumber))
    val maybeWinningBoard = updatedBoards.find(_.isAnyRowOrColumnComplete)
    maybeWinningBoard match {
      case Some(board) => (board, drawnNumber)
      case None => findWinningBoardAndNumber(numPosition + 1, updatedBoards)
    }

  val (winningBoard, winningNumber) = findWinningBoardAndNumber(0, boards)
  val result1 = winningBoard.unmarkedNumbers.sum * winningNumber

  println(result1)

  @tailrec
  def findLastWinningBoardAndNumber(
      numPosition: Int,
      notWinningBoards: Seq[Board],
      winningBoards: Seq[Board]
  ): (Board, Int) =
    val drawnNumber = drawnNumbers(numPosition)
    val updatedBoards = notWinningBoards.map(_.markNumber(drawnNumber))
    val newWinningBoards = updatedBoards.filter(_.isAnyRowOrColumnComplete)
    (newWinningBoards, updatedBoards) match {
      case (newlyWinningBoards, previouslyNotWinningBoards)
          if newlyWinningBoards.size == 1 && previouslyNotWinningBoards.size == 1 =>
        (newlyWinningBoards.head, drawnNumber)
      case (newlyWinningBoards, previouslyNotWinningBoards) =>
        findLastWinningBoardAndNumber(
          numPosition + 1,
          updatedBoards.filterNot(newlyWinningBoards.contains(_)),
          winningBoards ++ newlyWinningBoards
        )
    }

  val (lastWinningBoard, lastWinningNumber) =
    findLastWinningBoardAndNumber(0, boards, Nil)
  val result2 = lastWinningBoard.unmarkedNumbers.sum * lastWinningNumber
  println(result2)

end Puzzle4
