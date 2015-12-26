package bowling

case class Line(rolls: Seq[Int])

object Game {
  val TotalPins = 10
  val TotalFrames = 10

  def scoreFor(line: Line): Int = sum(line.rolls, TotalFrames)

  private def sum(rolls: Seq[Int], remainingFrames: Int): Int =
    if (remainingFrames == 0) 0
    else rolls match {
      case Seq(10, rest @_*) => 10 + strikeBonus(rest) + sum(rest, remainingFrames - 1)

      case Seq(first, second, rest @_*) if isSpare(first, second) =>
        first + second + spareBonus(rest) + sum(rest, remainingFrames - 1)

      case Seq(first, second, rest @_*) =>
        first + second + sum(rest, remainingFrames - 1)
    }

  private def isSpare(first: Int, second: Int): Boolean = {
    first + second == TotalPins
  }

  private def spareBonus(rest: Seq[Int]): Int = rest.head

  private def strikeBonus(nextRolls: Seq[Int]): Int = nextRolls.take(2).sum
}
