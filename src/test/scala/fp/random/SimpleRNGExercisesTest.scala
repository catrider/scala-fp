package fp.random

import fp.stream.StreamUtils.unfold
import org.scalatest.FunSuite

class SimpleRNGExercisesTest extends FunSuite {

  test("nonNegativeInt") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGExercises.nonNegativeInt(rng)))
      .take(100)
      .forAll(i => i >= 0))
  }

  test("double") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGExercises.double(rng)))
      .take(100)
      .forAll(i => i >= 0 && i <= 1))
  }

  test("intDouble") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGExercises.intDouble(rng)))
      .take(100)
      .forAll(result => {
        val (i, d) = result
        i >= 0 && d >= 0 && d <= 1
      }))
  }

  test("doubleInt") {
    assert(unfold(SimpleRNG(4):RNG)(rng => Some(SimpleRNGExercises.doubleInt(rng)))
      .take(100)
      .forAll(result => {
        val (d, i) = result
        i >= 0 && d >= 0 && d <= 1
      }))
  }

}
