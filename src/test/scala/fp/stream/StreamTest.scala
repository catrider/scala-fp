package fp.stream

object StreamTest {

  def main(args: Array[String]): Unit = {
    val a = Stream(
      {println(1); 1},
      {println(2); 2},
      {println(3); 3})
    println(a)
    println(a.toList)

    println("Taking 1")
    val b = Stream(
      {println(1); 1},
      {println(2); 2},
      {println(3); 3})
    println(b.take(1).toList)

    println("Taking 2")
    val c = Stream(
      {println(1); 1},
      {println(2); 2},
      {println(3); 3})
    println(c.take(2).toList)

    println("Dropping 2")
    val d = Stream({
      println(1); 1},
      {println(2); 2},
      {println(3); 3})
    println(d.drop(2).toList)

    println("Dropping 3")
    val e = Stream(
      {println(1); 1},
      {println(2); 2},
      {println(3); 3},
      {println(4); 4},
      {println(5); 5},
      {println(6); 6})
    println(e.drop(3).toList)

    println("take while even")
    val f = Stream(
      {println(2); 2},
      {println(4); 4},
      {println(6); 6},
      {println(7); 7},
      {println(8); 8},
      {println(10); 10})
    println(f.takeWhile(e => e % 2 == 0).toList)

    println("take while even 2")
    val g = Stream(
      {println(1); 1},
      {println(2); 2},
      {println(4); 4},
      {println(6); 6},
      {println(7); 7},
      {println(8); 8},
      {println(10); 10})
    println(g.takeWhile(e => e % 2 == 0).toList)
  }

}
