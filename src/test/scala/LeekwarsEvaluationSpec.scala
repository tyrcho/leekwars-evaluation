

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.Shrink._

@RunWith(classOf[JUnitRunner])
class LeekwarsEvaluationSpec extends FlatSpec with Matchers with PropertyChecks {
  val evaluation = new LeekwarsEvaluation

  "LW evaluation" should "return 0 for same teams" in {
    forAll { (team1: Seq[Leek]) =>
      evaluation.compare(team1, team1) shouldBe 0
    }
  }

  it should "return opposite for switched teams" in {
    forAll { (team1: Seq[Leek], team2: Seq[Leek]) =>
      evaluation.compare(team1, team2) shouldBe -evaluation.compare(team2, team1)
    }
  }

  it should "return >0 for team with more leeks" in {
    forAll(genTeam, genTeam) { (team1: Seq[Leek], more: Seq[Leek]) =>
      whenever(more.nonEmpty) {
        val better = team1 ++ more
        evaluation.compare(better, team1) should be > 0.0
      }
    }
  }

  it should "give better score to a team with less leeks but stronger" in {
    forAll(genTeam, genLeek) { (team1: Seq[Leek], leek: Leek) =>
      val t1 = team1 :+ leek
      val focused = team1 match {
        case h :: t => Leek(life = h.life + leek.life, damage = h.damage + leek.damage) :: t
      }
      evaluation.compare(focused, t1) should be > 0.0
    }
  }

  it should "return same sign when one team gets x2 damage and other gets x2 life" in {
    forAll(genTeam, genTeam, Gen.chooseNum(1, 5)) { (team1: Seq[Leek], team2: Seq[Leek], ratio) =>
      val t1 = team1.map(l => l.copy(damage = l.damage * ratio))
      val t2 = team2.map(l => l.copy(life = l.life * ratio))
      evaluation.compare(t1, t2).signum shouldBe evaluation.compare(team1, team2).signum
    }
  }

  it should "return >0 for team with a stronger leek (life)" in {
    forAll(genTeam, Gen.chooseNum(1, 1000)) { (team1: Seq[Leek], i: Int) =>
      whenever(team1.nonEmpty) {
        val l = team1.head
        val betterLeek = l.copy(life = l.life + i)
        val betterTeam = team1.tail :+ betterLeek
        evaluation.compare(betterTeam, team1) should be > 0.0
      }
    }
  }

  it should "return >0 for team with a stronger leek (damage)" in {
    forAll(genTeam, Gen.chooseNum(1, 1000)) { (team1: Seq[Leek], i: Int) =>
      whenever(team1.nonEmpty) {
        val l = team1.head
        val betterLeek = l.copy(damage = l.damage + i)
        val betterTeam = team1.tail :+ betterLeek
        evaluation.compare(betterTeam, team1) should be > 0.0
      }
    }
  }

  def genTeam: Gen[Seq[Leek]] = {
    for {
      n <- Gen.chooseNum(1, 8)
      l <- Gen.listOfN(n, genLeek)
    } yield l
  }

  def genLeek: Gen[Leek] =
    for {
      life <- Gen.chooseNum(1, 1000)
      damage <- Gen.chooseNum(1, 1000)
    } yield Leek(life, damage)

  implicit val arbitraryLeek = Arbitrary(genLeek)

  implicit def shrinkLeek: Shrink[Leek] = Shrink {
    case Leek(a, b) => for {
      (a1, b1) <- shrink((a, b))
    } yield Leek(a1, b1)
  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 15)

}