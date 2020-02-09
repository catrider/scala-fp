package fp.option

import fp.list._

object OptionUtilTest {

  def main(args: Array[String]): Unit = {
    println(OptionUtil.map2(Some(2), Some(2))((a, b) => a + b))
    println(OptionUtil.map2(None, Some(2))((a: Int, b) => a + b))
    println(OptionUtil.map2(Some(2), None)((a, b: Int) => a + b))
    println(OptionUtil.map2(None, None)((a: Int, b: Int) => a + b))

    println(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(Some(8), Nil)))))
    println(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(None, Nil)))))
    println(OptionUtil.sequence(Cons(None, Cons(Some(34), Cons(Some(8), Nil)))))
  }

}
