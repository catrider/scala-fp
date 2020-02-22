package fp.random

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, SimpleRNG) = {
    val newSeed = (seed * 0x5DEECE66DL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
