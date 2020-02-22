import fp.random.SimpleRNG

val rng = SimpleRNG(42)

val (n1, rng2) = rng.nextInt

val (n2, rng3) = rng2.nextInt

val (n2, _) = SimpleRNG(42).nextInt

val (n2, _) = SimpleRNG(42).nextInt