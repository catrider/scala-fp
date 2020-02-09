package fp

object OptionUtil {

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): fp.Option[C] = {
    a.flatMap(av => b.map(bv => f(av, bv)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a)
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    List.foldRight(a, Some(Nil:List[B]):Option[List[B]])((el, optionalList) => {
      (f(el), optionalList) match {
        case (Some(v), Some(l)) => Some(Cons(v, l))
        case (None, _) => None
        case (_, None) => None
      }
    })
  }

}
