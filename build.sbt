name := "aoc2018"

version := "1.0"

scalaVersion := "2.12.8"

commands += Command("p")
  { _ =>
    import complete.DefaultParsers._
    (' ' ~ charClass(_.isDigit, "digit").+.map(_.mkString.toInt)).map(_._2)
  }
  {
    case (previousState, i: Int) => Command.process("runMain problems.Problem" + "%02d".format(i), previousState)
  }