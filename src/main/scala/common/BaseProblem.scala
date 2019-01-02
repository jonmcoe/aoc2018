package common


import scala.io.Source

abstract class BaseProblem(day: Int, suffix: String = "") {

  def readInput(): Iterator[String] = Source.fromFile(s"data/p${"%02d".format(day) + suffix}").getLines()

  def solutionA: Any
  def solutionB: Any

  def main(args: Array[String]): Unit = {
    println(solutionA)
    println(solutionB)
  }
}
