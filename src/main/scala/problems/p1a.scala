package problems

import scala.io.Source

/**
  * Created by jon on 12/4/18.
  */
object p1a extends App {
  val lines = Source.fromFile("./data/p1").getLines()
  val integers = lines.map(_.toInt)
  println(integers.sum)
}
