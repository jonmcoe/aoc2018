package common


import scala.io.Source

abstract class BaseProblem(day: Int) extends App {

  private val formatted = "%02d".format(day)

  def readInput(): Iterator[String] = Source.fromFile(s"data/p$formatted").getLines()

  def solutionA: Any
  def solutionB: Any

  def printBoth: Unit = {
    println(solutionA)
    println(solutionB)
  }
}
