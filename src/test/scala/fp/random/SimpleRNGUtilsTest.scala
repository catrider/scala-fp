package fp.random

import fp.stream.StreamUtils.unfold
import org.scalatest.FunSuite

class SimpleRNGUtilsTest extends FunSuite {

  test("nonNegativeInt") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGUtils.nonNegativeInt(rng)))
      .take(100)
      .forAll(i => i >= 0))
  }

  test("double") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGUtils.double(rng)))
      .take(100)
      .forAll(i => i >= 0 && i <= 1))
  }

  test("intDouble") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGUtils.intDouble(rng)))
      .take(100)
      .forAll(result => {
        val (i, d) = result
        i >= 0 && d >= 0 && d <= 1
      }))
  }

  test("doubleInt") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGUtils.doubleInt(rng)))
      .take(100)
      .forAll(result => {
        val (d, i) = result
        i >= 0 && d >= 0 && d <= 1
      }))
  }

  test("mapDouble") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGUtils.mapDouble(rng)))
      .take(100)
      .forAll(i => i >= 0 && i <= 1))
  }

}
