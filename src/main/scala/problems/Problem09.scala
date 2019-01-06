package problems

import common.BaseProblem

object Problem09 extends BaseProblem (9) {

  private val regex = "(\\d+) players; last marble is worth (\\d+) points".r
  private val (numPlayers, lastScore) = readInput().next() match {
    case regex(p, s) => (p.toInt, s.toInt)
  }

  class Linked[A](val value: A, var previous: Linked[A], var next: Linked[A]) {
    def insertAfter(incoming: A): Unit = {
      val newNode = new Linked[A](incoming, this, next)
      next.previous = newNode
      next = newNode
    }

    def remove(): A = {
      previous.next = next
      next.previous = previous
      value
    }

    def debugPrint() : Unit = {
      val startVal = value
      var cur = this
      print(cur.value)
      while(cur.next.value != startVal) {
        print(s" - ${cur.next.value}")
        cur = cur.next
      }
      println()
    }

    def debugPrintBackwards() : Unit = {
      val startVal = value
      var cur = this
      print(cur.value)
      while(cur.previous.value != startVal) {
        print(s" - ${cur.previous.value}")
        cur = cur.previous
      }
      println()
    }
  }

  object Linked {
    def apply[A](initial: A): Linked[A] = {
      val linked = new Linked(initial, null, null)
      linked.previous = linked
      linked.next = linked
      linked
    }
  }

  def playGame(numPlayers: Int, lastScore: Int): Long = {
    val l = Linked(0)

    def playRound(incomingMarble: Int, player: Int, scoreMap: Map[Int, Long], currentPosition: Linked[Int]): Long = {
      if (incomingMarble > lastScore) scoreMap.values.max
      else if (incomingMarble % 23 == 0) {
        val toRemove = (0 to 7).foldLeft(currentPosition)((acc, _) => acc.previous)
        val removed = toRemove.remove()
        playRound(
          incomingMarble + 1, (player + 1) % numPlayers,
          scoreMap + (player -> (scoreMap.getOrElse(player, 0L) + incomingMarble + removed)), toRemove.next.next) // TODO: fix weird next.next
      }
      else {
        currentPosition.insertAfter(incomingMarble)
        playRound(incomingMarble + 1, (player + 1) % numPlayers, scoreMap, currentPosition.next.next) // TODO: fix weird next.next
      }
    }
    playRound(1, 1, Map.empty, l)
  }

  def solutionA = playGame(numPlayers, lastScore)
  def solutionB = playGame(numPlayers, lastScore * 100)
}
