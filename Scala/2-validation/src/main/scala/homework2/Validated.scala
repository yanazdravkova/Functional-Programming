package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case _ => false 
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case Invalid(_) => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = if (this.isValid) this else default 

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match {
    case (Valid(a1), Valid(a2)) => Valid((a1,a2))
    case (Valid(a1), Invalid(errors)) => Invalid(errors)
    case (Invalid(errors), Valid(a2)) => Invalid(errors)
    case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case i@Invalid(_) => i
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = ???

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = ???

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Invalid(errors) => invalid(errors)
    case Valid(a) => valid(a)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = ???

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = ???
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = ???
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = ???
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = ???
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = ???
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = ???
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = ???
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = ???
  }

  // ??? TODO: Add toValidated to Option instances
}
