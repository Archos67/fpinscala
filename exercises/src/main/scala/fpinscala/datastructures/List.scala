package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(a, as) => Cons(h, as)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case xs => {
      if(n > 0) {
        drop( tail(xs), n-1)
      } else {
        xs
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) {
        Cons(x, dropWhile(xs, f))
      } else {
        Cons(x, xs)
      }
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
    }

  def sumFoldL(ints: List[Int]): Int = foldLeft(ints, 0)((b: Int, a: Int) => a + b)

  def productFoldL(dbs: List[Double]): Double = foldLeft(dbs, 1.0)((b, a) => b * a)

  def lengthFoldL[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def reverse[A] (as : List [A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))


  def foldLeft_FoldR[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((a: A, b:B) => f(b, a))

  def foldRight_FoldL[A, B](l: List[A], z: B)(f: (A, B) => B) = foldLeft(l, z)((b: B, a: A) => f(a, b))

  def appendFoldl[A] (a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), foldLeft(reverse(a2), Nil: List[A])((xs, x) => Cons(x, xs)))((xs, x) => Cons(x, xs))

  def flatten[A](ls:List[List[A]]): List[A] = foldLeft(ls, Nil: List[A])(appendFoldl)

  def addOne(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def doubleToString(dbs: List[Double]): List[String] = dbs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons (a, as) => Cons(f(a), map(as)(f))
  }

  def filter[A] (as : List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) {
        Cons(x, filter(xs)(f))
      } else {
        filter(xs)(f)
      }
    }
  }

  def flatMap[A, B] (as: List[A])(f: A => List[B]): List[B] = flatten(map (as) (f))

  def filterFlatmap[A] (as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a: A) => if(f(a)) { List(a) } else { Nil})

  def addCorresponding (xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case Cons(a, as) => ys match {
      case Nil => xs
      case Cons(b, bs) => Cons(a + b, addCorresponding(as, bs))
    }
  }

  def zipWith[A, B, C] (as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
  }

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = {
    def hasSub (sup: List[A], sub: List[A], first : Boolean) : Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false // this case will not evaluate if sub is Nil
      case (Cons(x, xs), Cons(y, ys)) => {
        if (x == y) {
          hasSub(xs, ys, false)
        } else if (first) {
          hasSub(xs, Cons(y, ys), true)
        } else {
          false
        }
      }
    }
    hasSub(sup, sub, true)
  }
}
