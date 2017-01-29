import scala.util.Random

object Positioning extends App {
  val maxX = 20
  val maxY = 20
  var mine = Leeks(Seq.fill(4)(Pos(Random.nextInt(maxX / 2), Random.nextInt(maxY / 2))), 'M')
  val opp = Leeks(Seq.fill(4)(Pos(Random.nextInt(maxX / 2) + maxX / 2, Random.nextInt(maxY / 2) + maxY / 2)), 'O')
  val oppBary = Leeks(Seq(opp.barycentre), 'B')

  display(Seq(mine, opp, oppBary))
  for (i <- 1 to 20) {
    println("-" * maxX)

    mine = mine.surround(opp, 2, 5)
    display(Seq(mine, opp, oppBary))
  }

  def display(leeks: Seq[Leeks]) {
    for (y <- 0 until maxY) {
      print(s"$y	")
      for (x <- 0 until maxX) {
        val char = leeks.find(_.positions.exists(_.round == Pos(x, y))).map(_.char).getOrElse(' ')
        print(char)
      }
      println()
    }
  }

  case class Leeks(positions: Seq[Pos], char: Char) {
    val barycentre = positions.foldLeft(Pos(0, 0))(_ + _) / positions.size

    def surround(others: Leeks, speed: Int = 1, distance: Int = 5) = {
      def move(p: Pos) = {
        val target = others.positions.minBy(_.dist(barycentre))
        val closerThanFriends = p.dist(target) <= barycentre.dist(target) - 2
        val vectBP = target - p
        val ideal = target + vectBP * distance / vectBP.norm
        val vectPI = (p - ideal) * (if (closerThanFriends) -0.5 else 1)
        p + vectPI * (speed / vectPI.norm).min(1)
      }
      copy(positions = positions.map(p => move(p).round))
    }
  }
  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(p.x + x, p.y + y)
    def -(p: Pos) = Pos(p.x - x, p.y - y)
    def /(i: Double) = Pos(x / i, y / i)
    def *(i: Double) = Pos(x * i, y * i)
    def dist(p: Pos) = (p.x - x).abs + (p.y - y).abs
    def norm = dist(Pos(0, 0))
    def round = Pos(x.toInt, y.toInt)
  }
}