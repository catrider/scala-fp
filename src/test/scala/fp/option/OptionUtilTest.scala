package fp.option

import fp.list._
import org.scalatest.FunSuite

class OptionUtilTest extends FunSuite {

  test("map2") {
    assert(OptionUtil.map2(Some(2), Some(2))((a, b) => a + b) == Some(4))
    assert(OptionUtil.map2(None, Some(2))((a: Int, b) => a + b) == None)
    assert(OptionUtil.map2(Some(2), None)((a, b: Int) => a + b) == None)
    assert(OptionUtil.map2(None, None)((a: Int, b: Int) => a + b) == None)
  }

  test("sequence") {
    assert(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(Some(8), Nil)))) == Some(Cons(4, Cons(34, Cons(8, Nil)))))
    assert(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(None, Nil)))) == None)
    assert(OptionUtil.sequence(Cons(None, Cons(Some(34), Cons(Some(8), Nil)))) == None)
  }

}
