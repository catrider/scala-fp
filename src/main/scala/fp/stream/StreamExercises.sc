import fp.stream.Stream

val ones: fp.stream.Stream[Int] = Stream.cons(1, ones)
ones.take(3).toList

ones.exists(_ % 2 == 1)

ones.map(_ + 1).exists(_ % 2 == 0)

ones.takeWhile(_ == 1)

ones.forAll(_ != 1)