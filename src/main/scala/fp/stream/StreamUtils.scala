package fp.stream

import fp.stream.Stream.cons

object StreamUtils {

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(): Stream[Long] = {
    def _fibs(a: Long, b: Long): Stream[Long] = {
      cons(a + b, _fibs(b, a + b))
    }
    cons(0, cons(1, _fibs(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((nextEl, state)) => cons(nextEl, unfold(state)(f))
      case None => Empty
    }
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold(Nil)(_ => Some(a, Nil))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n)(lastEl => Some(lastEl, lastEl + 1))
  }

  def fibsUnfold() = {
    cons(0, cons(1, unfold((0, 1))(lastTwoNumbers => {
      val (secondMostRecentNumber, mostRecentNumber) = lastTwoNumbers
      val newNumber = mostRecentNumber + secondMostRecentNumber
      Some(newNumber, (mostRecentNumber, newNumber))
    })))
  }

}
