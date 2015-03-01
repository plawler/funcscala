package funcscala

/**
 * Created by paullawler on 2/20/15.
 */

object ChapterTwo {

  def abs(n: Int) = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    def doF(i: Int, acc: Int): Int = {
      if (i <= 0) acc
      else doF(i - 1, i * acc)
    }
    doF(n, 1)
  }

  def fib(n: Int): Int = {
    def doFib(cnt: Int, previous: Int, current: Int): Int = {
      if (cnt == 0) previous
      else doFib(cnt - 1, current, previous + current)
    }
    doFib(n, 0, 1)
  }

  private def formatAbs(n: Int) = {
    val s = s"The absolute value of $n is ${abs(n)}"
    println(s)
  }

  private def format(name: String, n: Int, f: Int => Int): String = {
    s"The $name of $n is ${f(n)}"
  }

  def main(args: Array[String]) {
    println(format("absolute value", -42, abs))
    println(format("factorial", 3, factorial))
    println(format("fibonacci", 7, fib))
    println(format("increment", 7, x => x + 1))
  }

}

object ChapterTwoDotSix {

  def binarySearch[A](somethings: Array[A], key: A, gt: (A,A) => Boolean): Int = {

    def doSearch(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val middle = (low + high) / 2
        val thing = somethings(middle)
        val greater = gt(thing, key)
        if (!greater && !gt(key, thing)) middle
        else if (greater) doSearch(low, middle, middle - 1)
        else doSearch(middle + 1, middle, high)
      }
    }
    doSearch(0, 0, somethings.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def doSort(current: Int, sorted: Boolean): Boolean = {
      if (current > as.length-1) sorted
      else if (gt(as(current), as(current-1))) doSort(current + 1, sorted: Boolean)
      else false
    }
    doSort(1, sorted = true)
  }

  /*
    partial1 takes a type A and a function f (which takes a type A and a type B and returns a C)
    and returns a function (which takes a type B and returns a type C)
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  /*
    curry takes a function f (which takes a type A and a type B and returns a type C)
    and returns (or rewrites it to) a function (which takes a type A
                                                and returns a function (which takes a type B
                                                                        and returns a type C)

    basically we are rewriting the function f such that instead of taking multiple arguments, it returns a
    function that takes a single parameter (a) which itself returns a function that takes a single parameter (b) which then returns the type C
    instead of f(a,b) we get f(a)(b)
   */
  def curry[A,B,C](f: (A,B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  /*
    compose takes a function f(which takes a type B and returns a type C) and function g(which takes a type A and returns a type B)
    and returns a function (which takes a type A and returns a type C)

    the ONLY way to implement this is by returning the composition of f and g. hence, the implementation is constrained by the type signature
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main (args: Array[String]) {
    val things = Array(21,1,8,3,5,13)
    val thingsStrings = Array("Frank", "Dick", "Harry", "Paul")

//    println(binarySearch(things, 13, (x: Int, y: Int) =>  x > y))
//    println(binarySearch(thingsStrings, "Frank", (x: String, y: String) =>  x.compareTo(y) > 0))

    println(isSorted(Array(2,1, 1,3), (x: Int, y: Int) => x >= y))
  }

}
