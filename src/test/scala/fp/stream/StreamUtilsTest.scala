package fp.stream

import org.scalatest.FunSuite

class StreamUtilsTest extends FunSuite {

  test("constant") {
    assert(StreamUtils.constant("b").take(4).toList == List("b", "b", "b", "b"))
  }

  test("from") {
    assert(StreamUtils.from(8).take(8).toList == List(8, 9, 10, 11, 12, 13, 14, 15))
  }

  test("fibs") {
    assert(StreamUtils.fibs().take(1).toList == List(0))
    assert(StreamUtils.fibs().take(2).toList == List(0, 1))
    assert(StreamUtils.fibs().take(3).toList == List(0, 1, 1))
    assert(StreamUtils.fibs().take(4).toList == List(0, 1, 1, 2))
    assert(StreamUtils.fibs().take(5).toList == List(0, 1, 1, 2, 3))
    assert(StreamUtils.fibs().take(6).toList == List(0, 1, 1, 2, 3, 5))
    assert(StreamUtils.fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    assert(StreamUtils.fibs().take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("constantUnfold") {
    assert(StreamUtils.constantUnfold("b").take(4).toList == List("b", "b", "b", "b"))
  }

  test("fromUnfold") {
    assert(StreamUtils.fromUnfold(8).take(8).toList == List(8, 9, 10, 11, 12, 13, 14, 15))
  }

}
