package fp.random

object SimpleRNGUtils {

  type Rand[+A] = RNG => (A, RNG)

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

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeEven)(i => i - i % 2)
  }

  def mapDouble(rng: RNG): (Double, RNG) = {
    val doubleRng = map(rng => rng.nextInt)(a => Math.abs(a.toDouble / Int.MaxValue.toDouble))
    doubleRng(rng)
  }

}
