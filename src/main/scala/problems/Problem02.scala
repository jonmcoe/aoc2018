package problems

import common.BaseProblem

object Problem02 extends BaseProblem(2) {
  val lines = readInput().toList

  def letterFrequencyMap(s: String, soFar: Map[Char, Int] = Map.empty): Map[Char, Int] =
    if (s.isEmpty) soFar
    else letterFrequencyMap(s.tail, soFar + (s.head -> (soFar.getOrElse(s.head, 0) + 1)))

  def numLinesContainingCharacterWithFrequency(n: Int) = lines.foldLeft(0)((i, l) => {
    if (letterFrequencyMap(l).values.exists(_ == n)) i + 1 else i
  })

  def differBy[A](a: Seq[A], b: Seq[A], n: Int): Boolean =
    if (a.length == b.length) a.zip(b).filter(t => t._1 != t._2).lengthCompare(n) == 0 else false

  def solutionA = numLinesContainingCharacterWithFrequency(2) * numLinesContainingCharacterWithFrequency(3)

  val differByOnePairs = lines.combinations(2).filter(pair => differBy(pair.head, pair.tail.head, 1))
  def solutionB = differByOnePairs.map(p =>
      p.head.zip(p.tail.head)
        .filter(t => t._1 == t._2)
        .map(_._1)
        .mkString)
      .next
}