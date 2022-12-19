package dev.cjens

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

class Board {
  val size: Int = 9
  val indices: Range = 0 until this.size
  val boxIndices: Range = 0 until math.sqrt(this.size).toInt
  var rows: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer()

  def fromExisting(b: Board): Board = {
    this.rows = b.rows.clone()
    this
  }

  def fromFile(fPath: String): Board = {
    val fBufSource: BufferedSource = Source.fromFile(fPath)
    val fIter: Iterator[String] = fBufSource.getLines()
    val fLines: ArrayBuffer[String] = ArrayBuffer()
    fIter.foreach(nextLine => fLines += nextLine)
    val validationPattern: Regex = ("[0-9]" + s"{${this.size}}").r
    for (line <- fLines) {
      line match {
        case validationPattern(_*) => this.rows += ArrayBuffer.from(line.toCharArray.map(_.toString.toInt))
        case _ => throw new IllegalArgumentException("Error reading board: invalid format")
      }
    }
    this
  }

  def setElementAt(row: Int, col: Int, value: Int) = {
    rows(row).update(col, value)
  }

  def getElements(): IndexedSeq[Int] = {
    this.getFlatIndexedSequence()
  }

  def getRow(i: Int): IndexedSeq[Int] = {
    this.rows(i).toIndexedSeq
  }

  def getRows(): IndexedSeq[IndexedSeq[Int]] = {
    val arrayBuffer: ArrayBuffer[IndexedSeq[Int]] = ArrayBuffer()
    for (i <- this.indices) arrayBuffer += getRow(i)
    arrayBuffer.toIndexedSeq
  }

  def getColumn(j: Int): IndexedSeq[Int] = {
    val transposed = this.rows.transpose
    transposed(j).toIndexedSeq
  }

  def getColumns(): IndexedSeq[IndexedSeq[Int]] = {
    val arrayBuffer: ArrayBuffer[IndexedSeq[Int]] = ArrayBuffer()
    for (i <- this.indices) arrayBuffer += getColumn(i)
    arrayBuffer.toIndexedSeq
  }

  def getBox(x: Int, y: Int): IndexedSeq[Int] = {
    val boxLen = math.sqrt(this.size).toInt
    val xIndices = (0 to 8).filter(i =>
      i >= (x / boxLen) * boxLen && i < (x / boxLen) * boxLen + boxLen)
    val yIndices = (0 to 8).filter(j =>
      j >= (y / boxLen) * boxLen && j < (y / boxLen) * boxLen + boxLen)
    val box: ArrayBuffer[Int] = ArrayBuffer()
    for {
      i <- xIndices
      j <- yIndices
    } {
      box.append(this.rows(i)(j).intValue)
    }
    box.toIndexedSeq
  }

  /** Returns an indexed sequence of all boxes in the board. */
  def getBoxes(): IndexedSeq[IndexedSeq[Int]] = {
    val arrayBuffer: ArrayBuffer[IndexedSeq[Int]] = ArrayBuffer()
    for {
      x <- this.boxIndices
      y <- this.boxIndices
    }
      arrayBuffer += getBox(x, y)
    arrayBuffer.toIndexedSeq
  }

  def setBoard(rows: ArrayBuffer[ArrayBuffer[Int]]): Boolean = {
    var didSucceed: Boolean = false
    if (rows == null) {
      this.rows = rows
      didSucceed = true
    }
    didSucceed
  }

  def setColumn(column_index: Int, column: ArrayBuffer[Int]): Boolean = {
    var didSucceed: Boolean = false
    val cols = this.rows.transpose
    if (column_index < this.size) {
      cols(column_index) = column
      this.rows = cols.transpose
      didSucceed = true
    }
    didSucceed
  }

  def setRow(row_index: Int, row: ArrayBuffer[Int]): Boolean = {
    var didSucceed: Boolean = false
    if (row_index < this.size) {
      this.rows(row_index) = row
      didSucceed = true
    }
    didSucceed
  }

  override def toString(): String = {
    val sb = new mutable.StringBuilder()
    if (this.rows == null) {
      sb.append("null")
    }
    else {
      for (row <- this.rows) {
        row.foreach(elem => sb.append(s"$elem"))
        sb.append("\n")
      }
    }
    sb.toString
  }

  private def getFlatIndexedSequence(): IndexedSeq[Int] = {
    this.rows.flatten.toIndexedSeq
  }
}
