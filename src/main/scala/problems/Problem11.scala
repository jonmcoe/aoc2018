package problems

import common.BaseProblem

object Problem11 extends BaseProblem (11) {

  private val gridSize = 300

  private val puzzleInput = readInput().next().toInt

  def cellScore(x: Int, y: Int): Int = {
    val rackID = x + 10
    (((rackID * y) + puzzleInput) * rackID / 100 % 10) - 5
  }

  private val cellTable: IndexedSeq[IndexedSeq[Int]] = (0 until gridSize).map(x => (0 until gridSize).map(y => cellScore(x, y)))

  def squareScore(x: Int, y: Int, size: Int): Int = {
    val included = for {
      i <- 0 until size
      j <- 0 until size
    } yield (x + i, y + j)
    included.map(t => cellScore(t._1, t._2)).sum
  }

  def allScores(maxSize: Int) = for {
    size <- 1 to maxSize
    x <- 1 to gridSize - size + 1
    y <- 1 to gridSize - size + 1
  } yield ((x, y, size), squareScore(x, y, size))

  def solutionA= allScores(3).maxBy(_._2)
  def solutionB = allScores(20).maxBy(_._2) // slow. maybe should fix origin and build size/score incrementally
}
