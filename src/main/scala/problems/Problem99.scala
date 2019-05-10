package problems

import common.BaseProblem

object Problem99 extends BaseProblem (99) {

  def permute(s: String, acc: String = ""): Seq[String] = {
    s match {
      case e if e.isEmpty => List(acc)
      case nonemptyS => nonemptyS.flatMap(c => permute(s.filter(_ != c), acc + c)) // TODO: zipWithIndex/view
    }
  }

  private val permutations = permute(readInput().next())

  def solutionA = permutations.size
  def solutionB = permutations.mkString("\n")
}