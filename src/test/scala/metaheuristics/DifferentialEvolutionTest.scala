package metaheuristics

import org.scalatest.{Matchers, FlatSpec}

class DifferentialEvolutionTest extends FlatSpec with Matchers {

  def parabola(x: Double): Double = x*x + 2*x + 3

  "Parabola" should "have minimum of 2" in {
    val DE = new DifferentialEvolution(N = 10)

    val bounds = List((-10.0, 10.0))

    val solution = DE.solve(x => parabola(x.head), bounds)

    parabola(solution.head) should be (2.0 +- 0.01)
  }

  def ackley(x: List[Double], a: Double = 20, b: Double = 0.2, c: Double = 2 * math.Pi): Double = {
    val c1 = -b * math.sqrt(x.map(v => v * v).sum / x.length)
    val c2 = x.map(v => math.cos(c * v)).sum / x.length

    // The Ackley function, taken from http://www.sfu.ca/~ssurjano/ackley.html
    -a * math.exp(c1) - math.exp(c2) + a + math.exp(1)
  }

  "The Ackley function" should "converge to origin" in {
    val DE = new DifferentialEvolution(N = 10)

    val bounds = List((-5.0, 5.0), (-5.0, 5.0))

    val solution = DE.solve(ackley(_), bounds)

    all (solution) should be (0.0 +- 0.01)
  }
}