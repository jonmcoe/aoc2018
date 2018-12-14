package problems

import common.BaseProblem

object Problem06 extends BaseProblem(6) {

  case class Cell(id: Int, x: Int, y: Int)

  val startingCells = readInput().toList.zipWithIndex.map { t =>
    val asList = t._1.split(',')
    Cell(t._2 + 1, asList.head.toInt, asList.tail.head.trim.toInt)
  }

  val grid = Array.ofDim[Int](startingCells.maxBy(_.x).x + 1, startingCells.maxBy(_.y).y + 1)
  startingCells.foreach(c => grid(c.x)(c.y) = c.id)

  val q = scala.collection.mutable.Queue[Cell]()
  q ++= startingCells
  while (q.nonEmpty) {
    val c = q.dequeue
    // TODO: must batch these writes so that we leave ties along
    if (c.x < grid.length - 1 && grid(c.x + 1)(c.y) == 0) {
      grid(c.x + 1)(c.y) = c.id
      q.enqueue(Cell(c.id, c.x + 1, c.y))
    }
    if (c.x > 0 && grid(c.x - 1)(c.y) == 0) {
      grid(c.x - 1)(c.y) = c.id
      q.enqueue(Cell(c.id, c.x - 1, c.y))
    }
    if (c.y < grid.length - 1 && grid(c.x)(c.y + 1) == 0) {
      grid(c.x)(c.y + 1) = c.id
      q.enqueue(Cell(c.id, c.x, c.y + 1))
    }
    if (c.y > 0 && grid(c.x)(c.y - 1) == 0) {
      grid(c.x)(c.y - 1) = c.id
      q.enqueue(Cell(c.id, c.x, c.y - 1))
    }
  }
  println(grid.deep.mkString("\n"))
  val excluded = Set(grid(0)(0), grid.last(0), grid(0).last, grid.last.last)
  val biggest = grid.flatten.groupBy(identity).maxBy(_._2.length)

  def solutionA = biggest._2.length
  def solutionB = ""

}
