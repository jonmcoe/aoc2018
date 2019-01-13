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
    var best = (Int.MinValue, -1)
    var incremental = 0

    val validSizeLimit = List(gridSize - x + 1, gridSize - y + 1, maxSize).min

    // this could probably be done with fold instead of mutables
    for(i <- 0 until validSizeLimit) {
      val addition1 = cellTable(y + i - 1).slice(x - 1, x + i - 1)
      val addition2 = (y to y + i).map(cy => cellTable(cy - 1)(x + i - 1))
      incremental += addition1.sum + addition2.sum
      if (incremental > best._1) {
        best = (incremental, i + 1)
      }
    }
    best
  }

  def allStartingPointBestScores(maxSize: Int) = for {
    x <- 1 to gridSize
    y <- 1 to gridSize
  } yield ((x, y), bestSquareScore(x, y, maxSize))

  def solutionA= allStartingPointBestScores(3).filter(_._2._2 == 3).maxBy(_._2._1)
  def solutionB = allStartingPointBestScores(300).maxBy(_._2._1)
}
