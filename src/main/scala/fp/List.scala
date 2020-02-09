package fp

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](ds: List[A]) = {
    ds match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](ds: List[A], el: A) = {
    Cons(el, List.tail(ds))
  }

  def drop[A](ds: List[A], n: Int): List[A] = {
    n match {
      case 0 => ds
      case x => drop(List.tail(ds), x-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) if !f(head) => l
      case Cons(head, tail) => dropWhile(tail, f)
    }
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h,t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((a, b) => a + b)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1D)(_ * _)
  }

  def length[A](ns: List[A]): Integer = {
    foldRight(ns, 0)((_, z) => z + 1)
  }

  def foldLeft[A,B](ns: List[A], z: B)(f: (B, A) => B): B = {
    ns match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def sumFoldLeft(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }

  def productFoldLeft(ns: List[Int]) = {
    foldLeft(ns, 1)(_ * _)
  }

  def lengthFoldLeft(ns: List[Int]) = {
    foldLeft(ns, 0)((z, _) => z + 1)
  }

  def reverse[A](ns: List[A]) = {
    foldLeft(ns, Nil:List[A])((reversedList, nextEl) => Cons(nextEl, reversedList))
  }

  def addOne(a1: List[Integer]): List[Integer] = {
    foldRight(a1, Nil:List[Integer])((nextEl, transformedList) => Cons(nextEl + 1, transformedList))
  }

  def eachDoubleToString(a1: List[Double]): List[String] = {
    foldRight(a1, Nil:List[String])((nextEl, transformedList) => Cons(nextEl.toString, transformedList))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((nextEl, transformedList) => Cons(f(nextEl), transformedList))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((nextEl, transformedList) => if (f(nextEl)) Cons(nextEl, transformedList) else transformedList)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil:List[B])((transformedList, nextEl) => append(transformedList, f(nextEl)))
  }

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(el => if (f(el)) List(el) else List())
  }

  def addCorrespondingElements(a1: List[Integer], a2: List[Integer]): List[Integer] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, addCorrespondingElements(tail1, tail2))
    }
  }

  def zip[A,B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zip(tail1, tail2)(f))
    }
  }
}
