package funcscala

/**
 * Created by paullawler on 2/25/15.
 */
sealed trait List[+A] // covariant parameter of list. iow, if Dog is a subtype of Animal, then List[Dog] is a subtype of List[Animal]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case (0.0, _) => 0.0
    case Cons(x, xs) =>  x * product(xs)
  }

}

object ChapterThree {

}