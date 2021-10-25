package maze

import maze.RichOps.MultiArrayOps

import scala.collection.mutable
import scala.util.Random

object Main extends App {
  val grid = Grid(10, 10)

  println(Sidewinder().on(grid))
}

case class Cell(row: Int, column: Int) {

  val links: mutable.Set[Cell] = mutable.Set()

  var north: Option[Cell] = None
  var south: Option[Cell] = None
  var east: Option[Cell] = None
  var west: Option[Cell] = None

  def link(cell: Cell, bidi: Boolean = true): Cell = {
    links += cell
    if (bidi) cell.link(this, false)
    this
  }

  def isLinked(cell: Cell): Boolean =
    links.contains(cell)
}

case class Grid(rows: Int, columns: Int, grid: Array[Array[Cell]]) {
  override def toString: String = {
    var output = "+" + "---+" * columns + "\n"

    grid.foreach { a =>
      var top = "|"
      var bottom = "+"

      a.foreach { c =>
        val eastBoundary = if (c.east.exists(c.isLinked)) " " else "|"
        val southBoundary = if (c.south.exists(c.isLinked)) "   " else "---"
        val body = "   "
        val corner = "+"

        top = s"$top$body$eastBoundary"
        bottom = s"$bottom$southBoundary$corner"
      }

      output = s"$output$top\n"
      output = s"$output$bottom\n"
    }

    output
  }
}

object RichOps {
  implicit class MultiArrayOps[A](val array: Array[Array[A]]) {
    def at(row: Int, col: Int): Option[A] =
      array.lift(row).flatMap(_.lift(col))
  }
}

object Grid {
  def apply(rows: Int, columns: Int): Grid = {
    val grid = (for (i <- 0 until rows) yield {
      (for (j <- 0 until columns) yield {
        Cell(i, j)
      }).toArray
    }).toArray

    grid.foreach(_.foreach({ c =>
      val row = c.row
      val col = c.column

      c.north = grid.at(row - 1, col)
      c.south = grid.at(row + 1, col)
      c.east = grid.at(row, col + 1)
      c.west = grid.at(row, col - 1)
    }))

    Grid(rows, columns, grid)
  }
}

object BinaryTree {
  def on(input: Grid): Grid = {
    input.grid.foreach(_.foreach { c =>
      val north = c.north
      val east = c.east

      Random.shuffle(List(north, east).flatten)
        .headOption
        .map(c.link(_))
        .getOrElse(c)
    })

    input
  }
}

class Sidewinder(rnd: => Boolean) {
  def on(input: Grid): Grid = {
    var collection = List.empty[Cell]

    input.grid.foreach(_.foreach { cell =>
      val north = cell.north
      val east = cell.east

      collection = collection :+ cell

      if (rnd || north.isEmpty) {
        List(east, north).flatten
          .headOption
          .map(cell.link(_))

        if (east.isEmpty) collection = List.empty

      } else {
        Random.shuffle(collection)
          .headOption
          .map(randomCell => randomCell.north.map(randomCell.link(_)))

        collection = List.empty
      }
    })

    input
  }
}

object Sidewinder {
  def apply(): Sidewinder = new Sidewinder(Random.nextBoolean())
}