package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i: Int, r: RNG) = rng.nextInt
    if(i == Int.MinValue) {
      (0, r)
    } else {
      (Math.abs(i: Int): Int, r)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i: Int, r: RNG) = nonNegativeInt(rng)
    ((i.toDouble) / (Int.MaxValue.toDouble + 1.0), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1 ) = nonNegativeInt(rng)
    val (d, r2 ) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1 ) = double(rng)
    val (i, r2 ) = nonNegativeInt(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, r) = nonNegativeInt(rng)
    if (count == 0) { (Nil, r) }
    else {
      val next = ints(count - 1)(r)
      (i :: next._1, next._2)
    }
  }

  def doubleMap: Rand[Double] = {
    map (nonNegativeInt) (_.toDouble / (Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs match {
        case h :: t => map2(h, sequence(t))(_ :: _)
        case Nil => unit(Nil)
      }
  }

  def intsSequence (count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan (n: Int): Rand[Int] =
      flatMap(nonNegativeInt) ({ i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
      })

  def mapFlat[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Flat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def main(args: Array[String]) = {
    val rng: RNG = Simple(540)
    println(nonNegativeInt(rng))
    println(doubleMap(rng))
    println(intDouble(rng))
    println(double3(rng))
    println(ints(5)(rng))
    println(intsSequence(5)(rng))
    println(nonNegativeLessThan(5)(rng))
  }
}

import State._

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => run(s) match {
      case (a, s2) => (f(a), s2)
    })
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap({ a => sb.flatMap({ b => State.unit(f(a,b)) })})

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => this.run(s) match {
      case (a, s2) => f(a).run(s2)
    })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.reverse.foldLeft (unit[S, List[A]](Nil)) (
      (sb, sa) => sa.map2(sb)(_ :: _)
    )
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S] (f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _ )) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, ca, co)) => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca-1, co)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence (inputs map ((i) =>  modify[Machine] (update (i))))
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    val m = Machine(true, 10, 0)
    println(simulateMachine(List(Coin, Turn)).run(m))
  }
}
