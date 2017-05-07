package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => Nil
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty[A]
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) ((a,b) => p(a) && b)

  def takeWhileFoldR(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) ((h, t) => if(p(h)) { cons(h, t) } else { empty[A]})

  def headOption: Option[A] = foldRight(None: Option[A]) ((h, _) => Some(h)) // I don't know why this is considered "Hard"

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) ((a, bs) => cons(f(a), bs))

  // Remove elements from List unless they satisfy a given condition
  def filter(f: A => Boolean): Stream[A] = foldRight (empty[A]) ((h, t) => if (f(h)) { cons(h,t) } else { t })

  def append[B>:A](a1: Stream[B]): Stream[B] = this.foldRight (a1) ((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B]) ((h, t) => f(h).append(t))


  // 5.13
  def mapUnfold[B] (f: A => B): Stream[B] =
    unfold (this) ({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def takeUnfold (n: Int): Stream[A] =
    unfold ((n, this)) ({
      case (n, Cons(h,t)) if n > 0 => Some(h(), (n-1, t()))
      case _ => None
    })

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    unfold (this) ({
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    })

  def zipWith[B, C] (bs: List[B])(f: (A, B) => C) =
    unfold ((this, bs)) ({
      case (Cons(ha, ta), hb :: tb) => Some((f(ha(), hb), (ta(), tb)))
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold ((this, s2)) ({
      case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
      case (Cons(ha, ta), _) => Some(((Some(ha()), None), (ta(), empty[B])))
      case (_, Cons(hb, tb)) => Some(((None, Some(hb())), (empty[A], tb())))
      case _ => None
    })


  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll (s) filter ({ case (_, None) => false; case _ => true }) forAll({
        case (Some(a), Some(b)) => a == b
        case _ => false
      })
  }

  def tails: Stream[Stream[A]] =
    unfold(this) ({
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty => None
    }) append Stream(empty[A])

  def scanRight[B] (acc: B) (f: (A, B) => B) : Stream[B] =
    foldRight(Stream(acc)) ({
      case (a, Cons(h, t)) => cons(f(a, h()), Cons(h, t))
    })
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[C](c: C): Stream[C] = cons(c, constant(c))


  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def fibonnaci(prev: Int , next: Int): Stream[Int] = {
      cons(prev + next, fibonnaci(next, prev + next))
    }
    cons(0, cons(1, fibonnaci(0, 1)))
  }
  // CoRecursive = produces data // guarded recursion // productivity = cotermination
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }
  }

  def fibsUnfold: Stream[Int] = {
    cons(0, cons(1, unfold (0, 1) (s => s match {
      case (prev, next) => Some((prev + next, (next, prev + next)))
    })))
  }

  def fromUnfold (start: Int): Stream[Int] =
    unfold(start) (s => Some(s, s + 1))

  def constantUnfold[C] (c: C): Stream[C] =
    unfold(c) (s => Some(s, s))

  def oneUnfold: Stream[Int] = constantUnfold(1)

}

object StreamTest {

  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5)
    // 5.2
    println(s drop 3 toList)
    println(s take 2 toList)
    // 5.3
    println(s takeWhile(_ => false) toList)
    println(s takeWhile(i => i < 3) toList)
    println(s takeWhile(i => i != 4) toList)
    // 5.4
    println(s forAll(i => i <3))
    println(s forAll(i => i > 0))
    //5.5
    println(s takeWhileFoldR (_ => false) toList)
    println(s takeWhileFoldR (i => i < 3)toList)
    println(s takeWhileFoldR (i => i != 4) toList)
    // 5.6
    println(Stream().headOption)
    println(s.headOption)
    // 5.7
    println(s.map(_ + 1).toList )
    println(s.filter((_ % 2 == 0)).toList)
    println(s.append(Stream(6,7)) toList)
    println(s flatMap (i => Stream(i* 1, i * 2, i * 3)) toList)

    println(Stream.ones.map(_ + 1).exists(_ % 2 == 0))
    Stream.ones.takeWhile(_ == 1) // ones?
    println(Stream.ones.forAll(_ != 1))
    // 5.9
    println(Stream.from(5) take(3) toList)
    // 5.10
    println(Stream.fibs.take(10) toList)
    // 5.12
    println(Stream.fibsUnfold.take(10) toList)
    println(Stream.fromUnfold(5) take(3) toList)
    println(Stream.constantUnfold(5) take(3) toList)
    println(Stream.oneUnfold take(3) toList)
    // 5.13
    println(Stream.fromUnfold(1) zipAll (Stream.fibs) take(5) toList) // Fibs with Order
    // 5.14
    println(Stream(1,2,3) startsWith( Stream(1,2)))
    println(Stream(1,2,3) startsWith( Stream(2,3)))
    println(Stream(1,2) startsWith( Stream(1,2,3)))
    // 5.15
    println(Stream(1,2,3).tails map (_.toList) toList)
    // 5.16
    println(Stream(1,2,3).scanRight(0)(_ + _) toList)

  }
}