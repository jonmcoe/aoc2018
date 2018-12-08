package problems

import scala.io.Source

/**
  * Created by jon on 12/7/18.
  */
object p2a extends App {
  val lines = Source.fromFile("./data/p2").getLines().toList
  def letterFrequencyMap(s: String, soFar: Map[Char, Int] = Map.empty): Map[Char, Int] = {
    if (s.isEmpty) {
      return soFar
    }
    letterFrequencyMap(s.tail, soFar + (s.head -> (soFar.getOrElse(s.head, 0) + 1)))
  }

  val twos = lines.foldLeft(0)((i, l) => {
    if (letterFrequencyMap(l).values.exists(_ == 2)) i + 1 else i
  })

  val threes = lines.foldLeft(0)((i, l) => {
    if (letterFrequencyMap(l).values.exists(_ == 3)) i + 1 else i
  })

  println(twos * threes)
}

object p2b extends App {

  // TOOD: can this b generalized by type or by n instead of one?
  def differByOne(a: String, b: String): Boolean = {
    if (a.length == b.length) {
      val upToFirstDifference = a.zip(b).dropWhile(t => t._1 == t._2)
      upToFirstDifference.nonEmpty && upToFirstDifference.tail.forall(t => t._1 == t._2)
    } else false
  }

  val lines = Source.fromFile("./data/p2").getLines().toList
  val differByOnePairs = lines.combinations(2).filter(pair => differByOne(pair.head, pair.tail.head))
  println(differByOnePairs.flatMap(p => p.head.zip(p.tail.head).filter(t => t._1 == t._2).map(_._1)).mkString)
}
