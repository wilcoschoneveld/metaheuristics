package metaheuristics

import scala.util.Random

class DifferentialEvolution(val N: Int = 1000, val F: Double = 0.5, val CR: Double = 0.1) {
  // Algorithm types
  type Member = List[Double]
  type Generation = Vector[Member]

  def mutate(index: Int, generation: Generation): Member = {
    // Create a random number stream with distinct values
    val selector = Stream.continually(Random.nextInt(N)).distinct

    // Select 3 mates from the generation
    val mates = selector.filter(_ != index).take(3).map(generation(_))

    // Calculate the donor member
    (mates(0), mates(1), mates(2)).zipped map {
      case (e1, e2, e3) => e1 + F * (e2 - e3)
    }
  }

  def recombine(member: Member, donor: Member): Member = {
    // Choose a parameter to always take from the donor
    val take = Random.nextInt(member.length)

    // Loop through all parameters and take with a probability
    member.zip(donor).zipWithIndex.map {
      case ((eM, eD), index) =>
        if (Random.nextDouble < CR || index == take) eD else eM
    }
  }

  def select(member: Member, trial: Member, f: Member => Double): Member = {
    // Select the best of both members
    if (f(trial) < f(member)) trial else member
  }

  def populate(bounds: List[(Double, Double)]): Generation = {
    // A list of random values within the given bounds
    def generateMember = bounds.map {
      case (min, max) => min + (max - min) * Random.nextDouble
    }

    // Fill a vector with N random generated members
    Vector.fill(N)(generateMember)
  }

  def evolve(f: Member => Double, generation: Generation): Stream[Generation] = {
    // Build a next generation member by member
    val next = generation.zipWithIndex.map {
      case (member, index) =>
        val donor = mutate(index, generation)
        val trial = recombine(member, donor)
        select(member, trial, f)
    }

    // Stream
    next #:: evolve(f, next)
  }
}