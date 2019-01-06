package problems

import common.BaseProblem

object Problem10 extends BaseProblem (10) {

  private val numberSection = " ?(-?\\d+)"
  private val regex = s"position=<$numberSection, $numberSection> velocity=<$numberSection, $numberSection>".r

  case class Star(posX: Int, posY: Int, velX: Int, velY: Int)

  private val stars : Set[Star] = readInput().map {
    case regex(sx, sy, vx, vy) => Star(sx.trim.toInt, sy.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }.toSet

  def starsAlign(starSet: Set[(Int, Int)]): Boolean =
    starSet.exists(
      r => (0 until 10).forall(i => starSet.contains((r._1, r._2 + i))) // look for vertical lines
    )

  def makeStarGrid(starSet: Set[(Int, Int)]) : String = {
    val minX = starSet.minBy(_._1)._1
    val maxX = starSet.maxBy(_._1)._1
    val minY = starSet.minBy(_._2)._2
    val maxY = starSet.maxBy(_._2)._2
    (minY to maxY).map(y => (minX to maxX).map(x => if (starSet.contains(x, y)) "*" else " ").mkString).mkString("\n")
  }

  def findMessage(stars: Iterable[Star], iterations: Int): (String, Int) = {
    val asSet = stars.map(s => (s.posX, s.posY)).toSet
    if (starsAlign(asSet)) (makeStarGrid(asSet), iterations)
    else
      findMessage(stars.map(s => Star(s.posX + s.velX, s.posY + s.velY, s.velX, s.velY)), iterations + 1)
  }

  private val jointSolution = findMessage(stars, 0)
  override def solutionA: Any = jointSolution._1
  override def solutionB: Any = jointSolution._2

}