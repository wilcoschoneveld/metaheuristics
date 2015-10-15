package metaheuristics

import org.scalatest.FunSuite

class DifferentialEvolutionTest extends FunSuite {

  def ackley(x: List[Double], a: Double = 20, b: Double = 0.2, c: Double = 2 * math.Pi): Double = {
    val c1 = -b * math.sqrt(x.map(v => v * v).sum / x.length)
    val c2 = x.map(v => math.cos(c * v)).sum / x.length

    // The Ackley function, taken from http://www.sfu.ca/~ssurjano/ackley.html
    -a * math.exp(c1) - math.exp(c2) + a + math.exp(1)
  }

  test("The Ackley function should converge") {
    val DE = new DifferentialEvolution(N = 100)

    val bounds = List((-5.0, 5.0), (-5.0, 5.0))

    val initial = DE.populate(bounds)

    val generations = DE.evolve(ackley(_), initial)

    val best = generations(1000).minBy(ackley(_))

    assert(best.map(e => e * e).sum < 0.001)
  }
}