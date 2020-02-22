package fp.random

trait RNG {

  def nextInt: (Int, RNG)

}
