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

}
