package problems

import common.BaseProblem

object Problem07 extends BaseProblem(7) {

  val secondsBase = 60
  val numberWorkers = 5

  val regex = "Step (.) must be finished before step (.) can begin.".r

  val dependencies = readInput().map {
    case regex(left, right) => (left.charAt(0), right.charAt(0))
  }.toList

  val ready = dependencies.map(_._1).toSet.diff(dependencies.map(_._2).toSet).toList

  def performSteps(stepsReady: Set[Char], stepsBlocked: Seq[(Char, Char)], stepsPerformed: List[Char]): List[Char] = {
    if (stepsReady.isEmpty) stepsPerformed else {
      val nextUp = stepsReady.min
      val (unblocked, stillBlocked) = stepsBlocked.partition(_._1 == nextUp)
      val newlyReady = unblocked.filter(u => !stillBlocked.map(_._2).contains(u._2)).map(_._2)
      performSteps(
        (stepsReady ++ newlyReady).filter(_ != nextUp),
        stillBlocked,
        stepsPerformed :+ nextUp)
    }
  }

  override def solutionA: Any = performSteps(ready.toSet, dependencies, List.empty).mkString("")

  case class ActiveStep(c: Char, timeStarted: Int) {
    def timeRequired: Int = secondsBase + c - 'A' + 1
    def isDone(now: Int): Boolean = now - timeStarted >= timeRequired
  }

  def parallelPerformSteps(stepsReady: List[Char], stepsBlocked: Seq[(Char, Char)], stepsPerformed: List[Char],
                           stepInProgress: Set[ActiveStep], currentTime: Int): Int = {
    if (stepsBlocked.isEmpty && stepInProgress.isEmpty) currentTime - 1 else {
      val (finished, stepsStillInProgress) = stepInProgress.partition(_.isDone(currentTime))

      val (unblocked, stillBlocked) = stepsBlocked.partition(t => finished.map(_.c).contains(t._1))
      val newlyReady = unblocked.filter(u => !stillBlocked.map(_._2).contains(u._2)).map(_._2)

      val nextUp = (stepsReady ++ newlyReady).sorted.take(numberWorkers - stepsStillInProgress.size)
      val nowInProgress = stepsStillInProgress ++ nextUp.map(ActiveStep(_, currentTime))
      val nowReady = ((stepsReady  ++ newlyReady).toSet diff nowInProgress.map(_.c)).toList

      parallelPerformSteps(
        nowReady,
        stillBlocked,
        stepsPerformed,
        nowInProgress,
        currentTime + 1)
    }
  }

  override def solutionB: Any = parallelPerformSteps(ready, dependencies, List.empty, Set.empty, 0)
}
