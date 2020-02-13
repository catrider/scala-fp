package fp.stream

import fp.option._

sealed trait Stream[+A] {

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, tail) => h() :: tail().toList
    }
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Empty else this match {
      case Empty => Empty
      case Cons(head, tail) => Cons(head, () => tail().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) this else this match {
      case Empty => Empty
      case Cons(_, tail) => tail().drop(n - 1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((nextEl, transformedStream) => Cons(() => f(nextEl), () => transformedStream))
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    def _append[B >: A](s1: Stream[B], s2: Stream[B]): Stream[B] = {
      s1 match {
        case Empty => s2
        case Cons(h, t) => Cons(h, () => _append(t(), s2))
      }
    }
    _append(this, s)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
