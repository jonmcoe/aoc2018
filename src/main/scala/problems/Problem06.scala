package problems

import common.BaseProblem

import scala.collection.mutable.ArrayBuffer

object Problem06 extends BaseProblem(6) {

  case class Cell(id: Int, x: Int, y: Int)

  val startingCells = readInput().toList.zipWithIndex.map { t =>
    val asList = t._1.split(',')
    Cell(t._2 + 1, asList.head.toInt, asList.tail.head.trim.toInt)
  }

  def printGrid(x: Array[Array[Option[Int]]]) = println(x.transpose.map(_.map(_.getOrElse(9))).deep.mkString("\n"))

  val grid = Array.fill[Option[Int]](startingCells.maxBy(_.x).x + 1, startingCells.maxBy(_.y).y + 1)(None)

  def fillGrid(newCells: Iterable[Cell]): Unit = {
    if (newCells.nonEmpty) {
      newCells.foreach(c => grid(c.x)(c.y) = Some(c.id))
      val allNeighbors = newCells.flatMap(c => {
        val incoming = ArrayBuffer.empty[Cell]
        if (c.x < grid.length - 1 && grid(c.x + 1)(c.y).isEmpty) {
          incoming += Cell(c.id, c.x + 1, c.y)
        }
        if (c.x > 0 && grid(c.x - 1)(c.y).isEmpty) {
          incoming += Cell(c.id, c.x - 1, c.y)
        }
        if (c.y < grid(0).length - 1 && grid(c.x)(c.y + 1).isEmpty) {
          incoming += Cell(c.id, c.x, c.y + 1)
        }
        if (c.y > 0 && grid(c.x)(c.y - 1).isEmpty) {
          incoming += Cell(c.id, c.x, c.y - 1)
        }
        incoming
      }).toSet
      val eligibleNeighbors = allNeighbors.groupBy(c => (c.x, c.y))
        .map(t => if (t._2.size == 1) t else (t._1, List(Cell(0, t._1._1, t._1._2)))).values.flatten
      println(eligibleNeighbors.toList.sortBy(_.id))
      fillGrid(eligibleNeighbors)
    }
  }
  fillGrid(startingCells)

  printGrid(grid)
  val excluded = grid(0).toSet ++ grid.last.toSet ++ grid.map(_(0)).toSet ++ grid.map(_.last) ++ Set(Some(0)).toSet
  val biggest = grid.flatten.filter(!excluded.contains(_)).groupBy(identity).maxBy(_._2.length)
  println(biggest)

  def solutionA = biggest._2.length
  def solutionB = ""

}
