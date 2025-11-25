import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object MonteCarlo {
  def integralMonteCarlo(f: Double => Double, l: Double, r: Double, pointsNumber: Int, threadsNumber: Int): Double = {
    require(l < r)
    require(pointsNumber > 0)
    require(threadsNumber > 0)
    // Предполагается, что f(x) >= 0 на [l, r]
    val maxY = math.max(f(l), f(r))
    val rectArea = (r - l) * maxY

    val pointsPerThread = pointsNumber / threadsNumber
    val remainder = pointsNumber % threadsNumber

    val tasks = (0 until threadsNumber).map { i =>
      val extra = if (i == 0) remainder else 0
      val count = pointsPerThread + extra
      Future {
        val rand = new Random()
        (0 until count).count { _ =>
          val x = l + rand.nextDouble() * (r - l)
          val y = rand.nextDouble() * maxY
          y <= f(x)
        }
      }
    }

    val totalUnder = Await.result(Future.sequence(tasks), Duration.Inf).sum
    rectArea * totalUnder.toDouble / pointsNumber
  }

  def main(args: Array[String]): Unit = {
    // Пример 1: f(x) = x
    val f1 = (x: Double) => x
    val res1 = integralMonteCarlo(f1, 0.0, 1.0, 200000, 4)
    println(s"Интеграл x на [0,1] ≈ $res1 (точное значение = 0.5)")

    // Пример 2: f(x) = x^2
    val f2 = (x: Double) => x * x
    val res2 = integralMonteCarlo(f2, 0.0, 1.0, 200000, 6)
    println(s"Интеграл x^2 на [0,1] ≈ $res2 (точное значение = 0.333...)")
  }
}