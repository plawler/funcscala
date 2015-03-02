package funcscala.chapterthree

/**
 * Created by paullawler on 2/25/15.
 */
sealed trait List[+A] // covariant parameter of list. iow, if Dog is a subtype of Animal, then List[Dog] is a subtype of List[Animal]

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
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("invalid argument: tail of empty list")
    case Cons(_, t) => t
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) l
    else drop(n-1, tail(l))
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

}

object ChapterThree extends App{

//  println(List.x)

//  println(List.tail(List(1,2,3)))
//  println(List.tail(Nil))

  println(List.drop(2, List(1,2,3,4)))

}