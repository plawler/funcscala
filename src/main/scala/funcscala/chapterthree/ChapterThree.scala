package funcscala.chapterthree

/**
 * Created by paullawler on 2/25/15.
 */
sealed trait List[+A]

// covariant parameter of list. iow, if Dog is a subtype of Animal, then List[Dog] is a subtype of List[Animal]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // variadic function
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _) // sugar. see product2 for alternative syntax

  def product2(l: List[Double]): Double = foldRight(l, 1.0)((x, y) => x * y)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("invalid argument: tail of empty list")
    case Cons(_, t) => t
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) l
    else drop(n - 1, tail(l))
  }

  val test = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case Cons(h, t) if !f(h) => Cons(h, dropWhile(t)(f))
    case _ => l
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("can't set head element of empty list")
    case Cons(_, t) => Cons(newHead, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("can't init an empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // just messing around trying to understand the splat operator
  def variadic(ints: Int*): List[Int] = {
    List(ints: _*)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

}

object ChapterThree extends App {
  //  println(List.x)

  println("tail " + List.tail(List(1, 2, 3)))
  //  println("tail with Nil " + List.tail(Nil))
  println("drop " + List.drop(2, List(1, 2, 3, 4)))
  println("dropWhile " + List.dropWhile(List(1, 2, 3, 4))(x => x % 2 == 0))
  println("setHead " + List.setHead(List(1, 2, 3, 4), 42))
  println("init " + List.init(List(1, 2, 3, 4)))
  println("variadic type " + List.variadic(1, 2, 3, 4))

  println("exercise 8 " +  List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

  println("length of list " + List.length(List(1,2,3)))
}

/*
  exercise 7
  foldRight cannot short circuit because we must continue to replace f's arguments since we don't call the function until we
  are completely finished evaluating those args. only until that point do we start calling f.
  */

/*
  exercise 8
  we get back the original list. foldRight replaces the Nil and Cons constructors of the list with the z (accumulator)
  and f (function) respectively. so we replace the Nil with Nil and the Cons with Cons(_,_) and we end up getting back the
  original list.
 */