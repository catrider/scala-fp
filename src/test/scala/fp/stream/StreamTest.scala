package fp.stream

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("Stream") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("take") {
    assert(Stream(1, 2, 3).take(1).toList == List(1))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
  }

  test("drop") {
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
    assert(Stream(1, 2, 3, 4, 5, 6).drop(3).toList == List(4, 5, 6))
  }

  test("takeWhile") {
    assert(Stream(2, 4, 6, 7, 8, 10).takeWhile(x => x % 2 == 0).toList == List(2, 4, 6))
    assert(Stream(1, 2, 4, 6, 7, 8, 10).takeWhile(x => x % 2 == 0).toList == List())
  }

}
