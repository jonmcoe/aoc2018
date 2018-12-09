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

  def differBy[A](a: Seq[A], b: Seq[A], n: Int): Boolean =
    if (a.length == b.length) a.zip(b).filter(t => t._1 != t._2).lengthCompare(n) == 0 else false

  val lines = Source.fromFile("./data/p2").getLines().toList
  val differByOnePairs = lines.combinations(2).filter(pair => differBy(pair.head, pair.tail.head, 1))
  val ans = differByOnePairs.map(p =>
      p.head.zip(p.tail.head)
        .filter(t => t._1 == t._2)
        .map(_._1)
        .mkString)

  println(ans.next) // problem states there is only one
}
