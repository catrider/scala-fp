package fp.stream

object StreamTest {

  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3)
    println(a)
    println(a.toList)
  }

}
