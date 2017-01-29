import scala.util.Random

object Positioning extends App {
  val maxX = 20
  val maxY = 20
  val mine = Leeks(Seq.fill(4)(Pos(Random.nextInt(maxX), Random.nextInt(maxY))), 'M')
  val opp = Leeks(Seq.fill(4)(Pos(Random.nextInt(maxX), Random.nextInt(maxY))), 'O')
  val oppBary = Leeks(Seq(opp.barycentre), 'B')

  display(Seq(mine, opp, oppBary))
  for (speed <- 1 to 20) {
    println("-" * maxX)

    display(Seq(mine.surround(opp, speed, 5), opp, oppBary))
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
        val vectBP = others.barycentre - p
        val ideal = others.barycentre + vectBP * distance / vectBP.norm
        val vectPI = p - ideal
        p + vectPI * (speed / vectPI.norm).min(1)
      }
      copy(positions = positions.map(move))
    }
  }
  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(p.x + x, p.y + y)
    def -(p: Pos) = Pos(p.x - x, p.y - y)
    def /(i: Double) = Pos(x / i, y / i)
    def *(i: Double) = Pos(x * i, y * i)
    def dist2(p: Pos) = (p.x - x) * (p.x - x) + (p.y - y) * (p.y - y)
    def dist(p: Pos) = math.sqrt(dist2(p))
    def norm = dist(Pos(0, 0))
    def round = Pos(x.toInt, y.toInt)
  }
}