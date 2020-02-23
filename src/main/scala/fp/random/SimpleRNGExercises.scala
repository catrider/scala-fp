package fp.random

object SimpleRNGExercises {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (number, nextRNG) = rng.nextInt
    (Math.abs(number % Int.MaxValue), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (number, nextRNG) = rng.nextInt
    (Math.abs(number.toDouble / Int.MaxValue.toDouble), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((d, i), rng2)
  }

}
