package laziness

import Stream._

sealed trait Stream[+A] { self =>
  def headOption: Option[A] = self match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], lst: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: lst)
      case Empty => lst
    }
    go(self, List[A]()).reverse
  }

  def take(n: Int): Stream[A] = self match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = self match {
    case Cons(_, t) if n >= 1 => t().drop(n-1)
    case _ => self
  }

  def exists(p: A => Boolean): Boolean = self match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    self.foldRight(true)((a, b) => p(a) && b)

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]){(a, b) =>
      if(f(a)) cons(a, b)
      else b
    }

  def append[B >: A](sb: => Stream[B]): Stream[B] =
    foldRight(sb)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def mapU[B](f: A => B): Stream[B] = unfold(self){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd:  => A, tl:  => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def sum(s: Stream[Int]): Int = {
    @annotation.tailrec
    def loop(current: Int, s: Stream[Int]): Int = s match {
      case Empty => current
      case Cons(h, t) => loop(current + h(), t())
    }
    loop(0, s)
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = {
      cons(first, go(second, first + second))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  def fibsU: Stream[Int] = unfold((0, 1)){case (x, y) => Some((x, (y, x + y)))}

  def fromU(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constantU(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

  def onesU: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def times(n: Int): Stream[Int] = unfold(1){
    x =>
      if(x <= n) Some(x, x + 1)
      else None
  }

}