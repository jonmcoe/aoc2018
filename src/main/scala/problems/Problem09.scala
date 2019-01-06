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

    def this(incoming: A) = {
      this(incoming, null, null)
      this.next = this
      this.previous = this
    }
  }

  def playGame(numPlayers: Int, lastScore: Int): Long = {
    val l = new Linked(0)

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
