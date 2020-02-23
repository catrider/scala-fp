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

}
