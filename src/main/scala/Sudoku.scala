/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Student Name: Cameron Jensen
 * Description: Prg 02 - Sudoku Puzzle
 */

import scala.collection.immutable.Nil.{flatten, iterator}
import scala.collection.{IterableFactory, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io._
import scala.collection.parallel.ParSeq
import scala.collection.parallel.CollectionConverters._
import dev.cjens.Board

object Sudoku {

  /** Returns a 2D array of Int representing a sudoku board given a filename. */
  def readBoard(fileName: String): Array[Array[Int]] = {
    val board: Board = new Board()
    board.fromFile(fileName)
    board.rows.map(_.toArray).toArray
  }

  /** Returns an array containing each box in board's array. */
  def getAllBoxes(board: Board): IndexedSeq[IndexedSeq[Int]] = {
    board.getBoxes()
  }

  /** Returns true if the sequence is valid, */
  def isValid(seq: IndexedSeq[Int]): Boolean = {
    (1 to 9).foreach(v => if (seq.count(_.equals(v)) > 1) return false)
    true
  }

  /** A board is valid if all of its rows, columns, and boxes are also valid. */
  def isValid(board: Board): Boolean = {
    if (allRowsValid(board) &&
      allColsValid(board) &&
      allBoxesValid(board))
      true
    else false
  }

  /** Returns true if all rows of the given board are valid sequences. */
  def allRowsValid(board: Board): Boolean = {
    for (row <- board.getRows())
      if (isValid(row))
        return false
    true
  }

  /** Returns true if all columns of the given board are valid sequences. */
  def allColsValid(board: Board): Boolean = {
    for (column <- board.getColumns())
      if (isValid(column))
        return false
    true
  }

  /** Return whether all boxes of the given board are valid sequences. */
  def allBoxesValid(board: Board): Boolean = {
    for (box <- board.getBoxes())
      if (isValid(box))
        return false
    true
  }

  /** Returns true if board is complete, that is, it contains no zeros. */
  def isComplete(board: Board): Boolean = {
    !board.getElements().contains(0)
  }

  /** True if the board is both complete and valid. */
  def isSolved(board: Board): Boolean = {
    if (isComplete(board) && isValid(board))
      true
    else
      false
  }

  def getChoices(board: Board): IndexedSeq[Board] = {
    val nextChoices = getNextChoicesFor(board)
    nextChoices
  }

  def getNextChoicesFor(board: Board): IndexedSeq[Board] = {
    val choices = mutable.IndexedBuffer[Board]()
    val zeroIndices = mutable.IndexedBuffer[(Int, Int)]()
    val allElems = board.getElements()
    allElems.zipWithIndex.foreach {
      elemAndIdx =>
        val value = elemAndIdx._1
        val index = elemAndIdx._2
        if (value.equals(0))
          zeroIndices.append((
            math.floorDiv(index, board.size),
            math.floorMod(index, board.size)))
    }
    // Get markup for every zero index
    val markedIndices = zeroIndices.map { rowCol =>
      (rowCol._1, rowCol._2, getCellMarkup(board, rowCol._1, rowCol._2))
    }
    print("debug\n")
    IndexedSeq()
  }

  /** Returns the Crook's algorithm markup for a given cell (valid values of x on [1, 9]) */
  def getCellMarkup(board: Board, row: Int, col: Int): IndexedSeq[Int] = {
    (1 to 9).filter(v =>
      !board.getBox(col, row).contains(v) &&
        !board.getColumn(col).contains(v) &&
        !board.getRow(row).contains(v)
    )
  }

  /** Return a solution to the puzzle (null if there is no solution). */
  def solve(board: Board): Board = {
    val choices: IndexedSeq[Board] = getChoices(board)
    val parChoices = choices.par.filter { parChoice =>
      isSolved(parChoice)
    }
    if (parChoices.nonEmpty) parChoices(0)
    else null
  }

  def countZeros(board: Board): Int = {
    board.getElements().count(_.equals(0))
  }

  def main(args: Array[String]): Unit = {
    val board: Board = new Board()
    if (args.length.equals(1))
      board.fromFile(args(0))
    else
      board.fromFile("boards/sudoku1.txt")
    val sol = solve(board)
    println(sol)
  }
}
