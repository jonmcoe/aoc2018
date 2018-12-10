package problems

import common.BaseProblem

object Problem03 extends BaseProblem(3) {

  case class Rectangle(id: Int, startX: Int, startY: Int, width: Int, height: Int)
  case class Cell(x: Int, y: Int)
  case class MarkedCell(id: Int, x: Int, y: Int)

  val lines = readInput().toList

  val parseLine = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  val rectangles = lines.map({
    case parseLine(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  })

  def generateCellSet(r: Rectangle): Set[Cell] = (for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield Cell(x, y)).toSet

  def generateMarkedCellSet(r: Rectangle): Seq[MarkedCell] = for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield MarkedCell(r.id, x, y)

  val takenInitial : Set[Cell] = Set.empty
  val overlapInitial : Set[Cell] = Set.empty
  val (taken, overlap) = rectangles.foldLeft(takenInitial, overlapInitial)((t: (Set[Cell], Set[Cell]), r: Rectangle) => {
    val newCells = generateCellSet(r)
    (t._1 ++ newCells, t._2 ++ t._1.intersect(newCells))
  })

  def solutionA = overlap.size

  def solutionB =
    rectangles
      .flatMap(generateMarkedCellSet)  // all marked cells
      .groupBy(mc => (mc.x, mc.y))     // group them by location. ex: (1,3) --> [(123, 1, 3), (456, 1, 3)
      .filter(_._2.length == 1)        // filter out those with any overlap
      .groupBy(_._2.head.id)           // group by id
      .find(t => {
        val rect = rectangles.find(_.id == t._1).get
        t._2.size == rect.width * rect.height
      }).get._1                       // select the id with number of entries equal to that of the initial rectangle
}
