/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Student Name: Cameron Jensen
 * Description: Prg 02 - Sudoku Puzzle
 */

import scala.collection.immutable.Nil.flatten
import scala.collection.{IterableFactory, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io._

import scala.collection.parallel.ParSeq
import scala.collection.parallel.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps

object Sudoku {

  /** Returns a 2D array of Int representing a sudoku board given a filename. */
  def readBoard(fileName: String, boardSize: Int = 9): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(fileName)
    val boardMtx: Array[Array[Int]] = Array.ofDim(boardSize, boardSize)
    val source = bufferedSource.getLines.mkString.filter(!_.equals(' '))
    val validationPattern = ("[0-9]" + s"{${math.pow(boardSize, 2).toInt}}").r
    source match {
      case validationPattern(_*) =>
        for {
          i <- 0 until boardSize
          j <- 0 until boardSize
        } boardMtx(i)(j) = source(boardSize * i + j).asDigit
      case _ => throw new IllegalArgumentException("Error reading board: invalid format")
    }
    boardMtx
  }

  /** Returns a String representation of a given sudoku board */
  def boardToString(board: Array[Array[Int]], addWhitespace: Boolean = false): String = {
    if (board == null) return "Null"
    val sb = new mutable.StringBuilder()
    for (x <- board) {
      if (addWhitespace) x.foreach(y => sb.append(s"$y\t"))
      else x.foreach(y => sb.append(s"$y"))
      sb.append('\n')
    }
    sb.toString
  }

  /** Returns a specific row from a sudoku board as a sequence of numbers. */
  def getRow(board: Array[Array[Int]], row: Int): Array[Int] = {
    board(row)
  }

  /** Return a specific column from a sudoku board as a sequence of numbers. */
  def getCol(board: Array[Array[Int]], col: Int): Array[Int] = {
    val transposed = board.transpose
    transposed(col)
  }

  /** Returns a specific box from a sudoku board as a sequence of numbers. */
  def getBox(board: Array[Array[Int]], x: Int, y: Int): Array[Int] = {
    val boxLen = math.sqrt(board.length).toInt
    val xBoxIndices = (0 to 8).filter(i =>
      i >= (x / boxLen) * boxLen && i < (x / boxLen) * boxLen + boxLen)
    val yBoxIndices = (0 to 8).filter(j =>
      j >= (y / boxLen) * boxLen && j < (y / boxLen) * boxLen + boxLen)
    val box: ArrayBuffer[Int] = ArrayBuffer()
    for {
      m <- xBoxIndices
      n <- yBoxIndices
    }
      box.append(board(m)(n).intValue)
    box.toArray
  }

  /** Returns an array containing each box in board's array. */
  def getAllBoxes(board: Array[Array[Int]]): Array[Array[Int]] = {
    val dim = math.sqrt(board.length).toInt
    val boxes: Array[Array[Int]] = Array.ofDim(board.length)
    for {
      x <- 0 until dim
      y <- 0 until dim
    } boxes(x * dim + y) = getBox(board, x, y)
    boxes
  }

  /** Returns true if the sequence is valid, that is, it contains 9 numbers in [0-9] with optionally repeating zeros. */
  def isValid(seq: Array[Int]): Boolean = {
    val patternCheckNumeric = raw"[0-9]{9}".r
    val matchString = seq.mkString
    matchString match {
      case patternCheckNumeric(_*) =>
        val matchSet = matchString.toSet
        if (matchSet.size.equals(9)) true
        else if (matchString.toList.count(_.equals('0')).equals(9 - matchSet.size)) false
        else true
      case _ => false
    }
  }

  /** A board is valid if all of its rows, columns, and boxes are also valid. */
  def isValid(board: Array[Array[Int]]): Boolean = {
    if (allRowsValid(board) &&
      allColsValid(board) &&
      allBoxesValid(board))
      true
    else false
  }

  /** Returns true if all rows of the given board are valid sequences. */
  def allRowsValid(board: Array[Array[Int]]): Boolean = {
    board.foreach { row => if (!isValid(row)) return false }
    true
  }

  /** Returns true if all columns of the given board are valid sequences. */
  def allColsValid(board: Array[Array[Int]]): Boolean = {
    board.indices.foreach { i =>
      if (!isValid(getCol(board, i))) return false
    }
    true
  }

  /** Return whether all boxes of the given board are valid sequences. */
  def allBoxesValid(board: Array[Array[Int]]): Boolean = {
    for {
      x <- 0 until math.sqrt(board.length).toInt
      y <- 0 until math.sqrt(board.length).toInt
    } {
      if (!isValid(getBox(board, x, y))) return false
    }
    true
  }

  /** Returns true if board is complete, that is, it contains no zeros. */
  def isComplete(board: Array[Array[Int]]): Boolean = {
    !board.flatten.contains(0)
  }

  /** True if the board is both complete and valid. */
  def isSolved(board: Array[Array[Int]]): Boolean = {
    if (isComplete(board) && isValid(board)) true
    else false
  }

  /** Return a new board configuration from the given one by setting a digit at a specific (row, col) location. */
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = {
    board(row)(col) = d
    board
  }

  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = {
    if (isComplete(board)) {
      return IndexedSeq(board)
    }
    val choices: ArrayBuffer[Array[Array[Int]]] = ArrayBuffer()
    for (thisRow <- board.indices) {
      for (thisColumn <- board.indices) {
        val newBoard: Array[Array[Int]] = board.map(_.clone())
        if (newBoard(thisRow)(thisColumn) == 0) {
          val row = getRow(newBoard, thisRow).clone
          val col = getCol(newBoard, thisColumn).clone
          val box = getBox(newBoard, thisRow, thisColumn).clone
          val validPredicate: Int => Boolean = v =>
            !row.contains(v) && !col.contains(v) && !box.contains(v)
          val validEntries = (1 to 9).filter(validPredicate)
          validEntries.foreach(e => choices.append(getChoice(newBoard.map(_.clone()), thisRow, thisColumn, e)))
        }
      }
    }
    choices.toIndexedSeq
  }

  /** Return a solution to the puzzle (null if there is no solution). */
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = {
    val choices = getChoices(board)
    if (isComplete(board)) {
      if (isSolved(board)) {
        return board
      }
      else {
        println(boardToString(board))
        return null
      }
    }
    else {
      println(s"Parallel: ${countZeros(board)}")
      choices.par.foreach {
        choice => solve(choice)
      }
    }
    null
  }

  /** Return a string formatted with all elements of array.
   * Note: Original in-line code provided by IntelliJ IDEA. */
  def formatStringFromArray[A](array: Array[A]): String = {
    array.mkString("[", ", ", "]")
  }

  def countZeros(array: Array[Array[Int]]): Int = {
    array.flatten.count(_.equals(0))
  }

  def main(args: Array[String]): Unit = {
    var board: Array[Array[Int]] = Array(Array())
    if (args.length.equals(1)) board = readBoard(args(0))
    else board = readBoard("boards/sudoku1.txt")
    val sol = solve(board)
    println(boardToString(sol))
  }
}
