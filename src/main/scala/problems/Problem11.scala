package problems

import common.BaseProblem

object Problem11 extends BaseProblem (11) {

  private val gridSize = 300

  private val puzzleInput = readInput().next().toInt

  def cellScore(x: Int, y: Int): Int = {
    val rackID = x + 10
    (((rackID * y) + puzzleInput) * rackID / 100 % 10) - 5
  }

  private val cellTable: IndexedSeq[IndexedSeq[Int]] = (1 to gridSize).map(y => (1 to gridSize).map(x => cellScore(x, y)))

  def bestSquareScore(x: Int, y: Int, maxSize: Int): (Int, Int) = {
    val validSizeLimit = List(gridSize - x + 1, gridSize - y + 1, maxSize).min
    val best = (0 until validSizeLimit).foldLeft(Int.MinValue, 0, 0) {
      (acc, i) => {
        val addition1 = cellTable(y + i - 1).slice(x - 1, x + i - 1)
        val addition2 = (y to y + i).map(cy => cellTable(cy - 1)(x + i - 1))
        val cur = acc._2 + addition1.sum + addition2.sum
        if (cur > acc._1)
          (cur, cur, i + 1)
        else
          (acc._1, cur, acc._3)
      }
    }
    (best._1, best._3)
  }

  def allStartingPointBestScores(maxSize: Int) = for {
    x <- 1 to gridSize
    y <- 1 to gridSize
  } yield ((x, y), bestSquareScore(x, y, maxSize))

  def solutionA= {
    val res = allStartingPointBestScores(3).filter(_._2._2 == 3).maxBy(_._2._1)
    s"${res._1._1},${res._1._2}"
  }
  def solutionB = {
    val res = allStartingPointBestScores(300).maxBy(_._2._1)
    s"${res._1._1},${res._1._2},${res._2._2}"
  }
}
