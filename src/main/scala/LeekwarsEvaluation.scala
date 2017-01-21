

class LeekwarsEvaluation {
  def compare(team1: Seq[Leek], team2: Seq[Leek]): Double = {
    score(team1) - score(team2)
  }

  def score(team: Seq[Leek]) = {
    team.map(l => l.damage * l.life).sum
  }
}

case class Leek(damage: Int, life: Int)