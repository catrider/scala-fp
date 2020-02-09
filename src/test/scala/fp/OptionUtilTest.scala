package fp

object OptionUtilTest {

  def main(args: Array[String]): Unit = {
    println(OptionUtil.map2(fp.Some(2), fp.Some(2))((a, b) => a + b))
    println(OptionUtil.map2(fp.None, fp.Some(2))((a: Int, b) => a + b))
    println(OptionUtil.map2(fp.Some(2), fp.None)((a, b: Int) => a + b))
    println(OptionUtil.map2(fp.None, fp.None)((a: Int, b: Int) => a + b))

    println(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(Some(8), Nil)))))
    println(OptionUtil.sequence(Cons(Some(4), Cons(Some(34), Cons(None, Nil)))))
    println(OptionUtil.sequence(Cons(None, Cons(Some(34), Cons(Some(8), Nil)))))
  }

}
