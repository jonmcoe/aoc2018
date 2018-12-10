package common


import scala.io.Source

abstract class BaseProblem(day: Int) {

  private val formatted = "%02d".format(day)

  def readInput(): Iterator[String] = Source.fromFile(s"data/p$formatted").getLines()

  def solutionA: Any
  def solutionB: Any

  def main(args: Array[String]): Unit = {
    println(solutionA)
    println(solutionB)
  }
}
