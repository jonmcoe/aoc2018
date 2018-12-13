package problems

import common.BaseProblem
import java.time.format.DateTimeFormatter
import java.time.{ LocalDateTime, ZoneOffset}


object Problem04 extends BaseProblem(4) {

  trait DatedEvent { val date: LocalDateTime }
  case class GuardStartsShiftEvent(date: LocalDateTime, id: Int) extends DatedEvent
  case class FallsAsleepEvent(date: LocalDateTime) extends DatedEvent
  case class WakesUpEvent(date: LocalDateTime) extends DatedEvent

  def parseTime(s: String): LocalDateTime =
    LocalDateTime.parse(s, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))

  case class GuardMinutePair(GuardId: Int, Minute: Int)
  type GuardMinuteFrequencyMap = Map[GuardMinutePair, Int]
  def timeSleptByGuardMinute(remainingEvents: List[DatedEvent], acc: GuardMinuteFrequencyMap, activeGuard: Option[Int], fallAsleepMinute: Option[Int]): GuardMinuteFrequencyMap = {
    if (remainingEvents.isEmpty) acc else {
      val current :: rest = remainingEvents
      current match {
        case GuardStartsShiftEvent(_, id) => timeSleptByGuardMinute(rest, acc, Some(id), fallAsleepMinute)
        case FallsAsleepEvent(date) => timeSleptByGuardMinute(rest, acc, activeGuard, Some(date.getMinute))
        case WakesUpEvent(date) =>
          val newAcc = (fallAsleepMinute.get until date.getMinute)
            .foldLeft(acc)((innerAcc, min: Int) => innerAcc + (GuardMinutePair(activeGuard.get, min) -> (innerAcc.getOrElse(GuardMinutePair(activeGuard.get, min), 0) + 1)))
          timeSleptByGuardMinute(rest, newAcc, activeGuard, None)
      }
    }
  }

  private val dateRegex = "\\[(.+)\\] (.+)".r
  private val guardRegex = "Guard #([0-9]+) begins shift".r
  private val fallsAsleepRegex = "falls asleep".r
  private val wakesUpRegex = "wakes up".r

  private val events: List[DatedEvent] = readInput().map {
    case dateRegex(dateStr, other) =>
      val date = parseTime(dateStr)
      other match {
        case guardRegex(id) => GuardStartsShiftEvent(date, id.toInt)
        case fallsAsleepRegex() => FallsAsleepEvent(date)
        case wakesUpRegex() => WakesUpEvent(date)
      }
  }.toList.sortBy(_.date.toEpochSecond(ZoneOffset.UTC))

  private val sleepinessMap = timeSleptByGuardMinute(events, Map.empty, None, None)
  private val sleepiestGuardInfo = sleepinessMap.groupBy(_._1.GuardId).maxBy(g => g._2.values.sum)

  def solutionA = sleepiestGuardInfo._1 * sleepiestGuardInfo._2.maxBy(_._2)._1.Minute
  def solutionB = sleepinessMap.maxBy(_._2)._1.GuardId * sleepinessMap.maxBy(_._2)._1.Minute
}
