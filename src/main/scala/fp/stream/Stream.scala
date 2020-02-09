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
      case Cons(head, tail) => Cons(head, () => tail().take(n-1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) this else this match {
      case Empty => Empty
      case Cons(_, tail) => tail().drop(n-1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(head, tail) if p(head()) => Cons(head, () => tail().takeWhile(p))
      case Cons(head, _) if !p(head()) => Empty
    }
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
