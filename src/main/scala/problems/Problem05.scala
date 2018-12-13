package problems

import common.BaseProblem


object Problem05 extends BaseProblem(5) {

  private val originalPolymer = readInput().next

  // TODO benchmark and see if this is better or worse
//  val eliminationPairs = ('a' to 'z').zip('A' to 'Z').flatMap(p => List(p._1.toString + p._2.toString, p._2.toString + p._1.toString))

  def result(cur: String, prev: String): String = {
    if (cur == prev) cur else {
      val toRemove = cur.sliding(2).filter(pair => pair.head != pair.tail.head && pair.head.toLower == pair.tail.head.toLower)
      result(toRemove.foldLeft(cur)((acc, replace) => acc.replace(replace, "")), cur)
    }
  }

  def solutionA = result(originalPolymer, "").length
  def solutionB = ('a' to 'z').map(c => result(originalPolymer.filter(_.toLower != c), "").length).min
}