import scala.util.Random
import math._

object Positioning extends App {
  val maxX = 40
  val maxY = 40
  case class Corner(x: Boolean, y: Boolean) {
    def genPos = {
      val rx = Random.nextInt(maxX / 2)
      val ry = Random.nextInt(maxY / 2)
      Pos(rx + (if (x) maxX / 2 else 0), ry + (if (y) maxY / 2 else 0))
    }
    def opposite = Corner(!x, !y)
  }
  val myCorner = Corner(Random.nextBoolean, Random.nextBoolean)
  var mine = Leeks(Seq.fill(4)(myCorner.genPos), 'M')
  val opp = Leeks(Seq.fill(4)(myCorner.opposite.genPos), 'O')
  val oppBary = Leeks(Seq(opp.barycentre), 'B')

  //  for (x <- -1 to 1; y <- -1 to 1) println(x, y, Pos(x, y).angle)

  for (i <- 0 to 20) {
    val myBary = Leeks(Seq(mine.barycentre), 'A')
    display(Seq(mine, opp, oppBary, myBary))

    println("-" * maxX)
    mine = mine.surround(opp, 3, 5)
  }

  def display(leeks: Seq[Leeks]) {
    for (y <- 0 until maxY) {
      print(s"$y	|")
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
      val target = others.positions.minBy(_.dist(barycentre))
      val vectBT = target - barycentre
      val ideal = target - vectBT * distance / vectBT.norm
      val angles = Seq(-1.5, -0.5, 0.5, 1.5).map(r => Math.PI + r * Math.PI / 6)
      val ideals = angles.map(a => ideal.rotate(a, target))
      def leekAngleToTarget(leek: Pos) = (leek - target).angle
      def move(p: Pos, i: Int) = {
        if (positions.exists(_.dist(target) > p.dist(target) + speed)) p
        else p.moveTo(ideals(i), speed)
      }
      copy(positions = positions.zipWithIndex.map { case (p, i) => move(p, i).round })
    }
  }
  case class Pos(x: Double, y: Double) {
    def +(p: Pos) = Pos(x + p.x, y + p.y)
    def -(p: Pos) = Pos(x - p.x, y - p.y)
    def /(i: Double) = Pos(x / i, y / i)
    def *(i: Double) = Pos(x * i, y * i)
    def dist(other: Pos): Double = sqrt(dist2(other))
    def dist2(other: Pos): Double = sqr(this.x - other.x) + sqr(this.y - other.y)

    def norm = dist(Pos(0, 0))
    def round = Pos(x.toInt, y.toInt)
    def angle = math.atan2(y, x)
    def rotate(alpha: Double, center: Pos) = {
      val p = center - this
      val beta = p.angle + alpha
      val r = p.norm
      val nx = r * math.cos(beta)
      val ny = r * math.sin(beta)
      center + Pos(nx, ny)
    }
    def scalar(p: Pos) = x * p.x + y * p.y

    def moveTo(tgt: Pos, distance: Double) =
      if (dist(tgt) <= distance) tgt
      else {
        val direction = tgt - this
        val distToTgt = sqrt(direction scalar direction)
        val delta = direction * (distance / distToTgt)
        this + delta
      }.round

    def sqr(x: Double) = x * x

  }

}