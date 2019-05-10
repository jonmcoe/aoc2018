package problems

import common.BaseProblem

object Problem99 extends BaseProblem (99) {

  def permute[A](s: Seq[A], acc: Seq[A] = Seq.empty): Seq[Seq[A]] = {
    s match {
      case Nil => List(acc)
      case _ => s
        .view
        .zipWithIndex
        .flatMap {
          case (c, i) => permute(s.take(i) ++ s.drop(i + 1), acc :+ c)
        }
    }
  }

  private val permutations = permute(readInput().next())

  def solutionA = permutations.size
  def solutionB = permutations.map(_.mkString).mkString("\n")
}