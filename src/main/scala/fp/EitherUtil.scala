package fp

object EitherUtil {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    List.foldRight(as, Right(Nil:List[B]):Either[E, List[B]])((el, optionalList) => {
      (f(el), optionalList) match {
        case (Right(v), Right(l)) => Right(Cons(v, l))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }
    })
  }

}
