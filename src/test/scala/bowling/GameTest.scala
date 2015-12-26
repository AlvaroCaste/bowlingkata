package bowling

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, ShouldMatchers}

class GameTest extends FlatSpec with ShouldMatchers with PropertyChecks {

  val rolls = Gen.chooseNum(0, Game.TotalPins)

  val mediocreFrames = for {
    firstRoll <- Gen.chooseNum(0, Game.TotalPins - 1)
    secondRoll <- Gen.chooseNum(0, Game.TotalPins - firstRoll - 1)
  } yield Seq(firstRoll, secondRoll)

  val mediocreLines = Gen.listOfN(Game.TotalFrames, mediocreFrames)
    .map(rolls => Line(rolls.flatten))

  val spareFrames = for {
    firstRoll <- Gen.chooseNum(0, Game.TotalPins - 1)
    secondRoll = Game.TotalPins - firstRoll
  } yield Seq(firstRoll, secondRoll)

  def lineWithNonFinalSpareFrame(bonus: Int) = for {
    spareFrameIndex <- Gen.chooseNum(0, Game.TotalFrames - 2)
    framesBeforeSpare <- Gen.listOfN(spareFrameIndex, mediocreFrames)
    spareFrame <- spareFrames
    bonusFrame = Seq(bonus, 0)
    framesAfterSpare <- Gen.listOfN(Game.TotalFrames - spareFrameIndex - 2, mediocreFrames)
  } yield Line(framesBeforeSpare.flatten ++ spareFrame ++ bonusFrame ++ framesAfterSpare.flatten)

  val lastSpareFrames = for {
    frame <- spareFrames
    bonusRoll <- rolls
  } yield frame :+ bonusRoll

  "Bowling score" should "be 0 for the gutter game" in {
    val gutterLine = Line(Seq.fill(20)(0))
    Game.scoreFor(gutterLine) shouldBe 0
  }

  it should "be the sum of rolls if no spare or strike is achieved" in {
    forAll(mediocreLines) { mediocreLine =>
      withClue(mediocreLine) {
        Game.scoreFor(mediocreLine) shouldBe mediocreLine.rolls.sum
      }
    }
  }

  it should "consider a spare in a non-final frame" in {
    val bonus = 7
    forAll(lineWithNonFinalSpareFrame(bonus)) { line =>
      Game.scoreFor(line) shouldBe line.rolls.sum + bonus
    }
  }

  it should "consider a spare in the last frame" in {
    val rollAfterSpare = 5
    val line = Line(Seq.fill(18)(2) ++ Seq(1, 9, rollAfterSpare))
    Game.scoreFor(line) shouldBe line.rolls.sum
  }

  it should "be 300 for the perfect game" in {
    val perfectLine = Line(Seq.fill(13)(Game.TotalPins))
    Game.scoreFor(perfectLine) shouldBe 300
  }
}
