package problems

import common.BaseProblem

object Problem01 extends BaseProblem(1) {
  val integers = readInput().map(_.toInt).toList
  val stream = Stream.continually(integers.toStream).flatten

  def findFirstRepeat(sum: Int = 0, seen: Set[Int] = Set.empty, it: Stream[Int] = stream, iterations: Int = 0): Int =
    if (seen.contains(sum)) sum else findFirstRepeat(sum + it.head, seen + sum, it.tail, iterations + 1)

  def solutionA = integers.sum
  def solutionB = findFirstRepeat()
}
