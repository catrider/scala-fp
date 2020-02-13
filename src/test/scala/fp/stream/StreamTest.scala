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

  test("exists") {
    assert(Stream(1, 2, 3, 5, 7).exists(a => a % 2 == 0) == true)
    assert(Stream(1, 3, 5, 7).exists(a => a % 2 == 0) == false)
  }

  test("forAll") {
    assert(Stream(1, 2, 3, 5, 7).forAll(a => a % 2 == 1) == false)
    assert(Stream(1, 3, 5, 7).forAll(a => a % 2 == 1) == true)
  }

  test("map") {
    assert(Stream(1, 2, 3, 4, 5).map(x => x + 3).toList == Stream(4, 5, 6, 7, 8).toList)
    assert(Stream(1, 2, 3, 4, 5).map(x => x * 3).toList == Stream(3, 6, 9, 12, 15).toList)
    assert(Stream(1, 2, 3, 4, 5).map(x => x % 2 == 1).toList == Stream(true, false, true, false, true).toList)
  }

  test("append") {
    assert(Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8)).toList == Stream(1, 2, 3, 4, 5, 6, 7, 8).toList)
  }

}
