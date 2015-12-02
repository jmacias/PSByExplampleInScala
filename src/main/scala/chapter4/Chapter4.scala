package chapter4

import scala.annotation.tailrec

object Chapter4 {

  // Recursion, Maps and  Folds

  // If we apply the annotation scalac complains
  // that the fact call is not in tail position
  //@tailrec
  def fact(n:Int):Int = n match {
    case 0 => 1
    case n => n * fact(n-1)
  }

  // A definition of factorial, using a local, tail recursive function
  // From https://github.com/fpinscala/fpinscala
  def factorial(n: BigDecimal): BigDecimal = {
    @tailrec
    def go(n: BigDecimal, acc: BigDecimal): BigDecimal =
      if (n <= 0) acc // End Branch
      else go(n-1, n*acc) // Reducing Branch

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  //let pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)
  def pairs(n:Int) = (1 to n) flatMap (i => (1 to i) map (j => (i,j)))

  //let factors n = filter (\pair -> product pair == n) (pairs n)
  def factors(n:Int) = pairs(n) filter (p => (p._1 * p._2) == n)

  def factors2(n:Int) = (for {
    i <- 1 to n
    j <- 1 to i
  } yield (i,j)) filter (p => (p._1 * p._2) == n)

  def factors3(n:Int) = for {
    i <- 1 to n
    j <- 1 to i if i * j == n
  } yield (i,j)


//  reverse :: forall a. [a] -> [a]
//  reverse = reverse' []
//  where
//  reverse' acc [] = acc
//  reverse' acc (x : xs) = reverse' (x : acc) xs

  def reverse[A](xs: List[A]): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], rem: List[A]): List[A] = rem match {
      case Nil => acc
      case h :: rest => loop(h :: acc, rest)
    }
    loop(Nil, xs)
  }

//  let reverse :: forall a. [a] -> [a]
//    reverse = foldr (\x xs -> xs ++ [x]) []

  def reverseFold[A](xs: List[A]): List[A] =
    xs.foldRight(List[A]()) { (x, acc) =>  acc ::: x :: Nil }

  def main(args: Array[String]) {
    import scalaz._
    import Scalaz._

    println("Chapter 4: Recursion, Maps and  Folds")
    println(fact(3)) //=> 6
    //But this throws a StackOverflow
    //println(fact(30000))

    //A correct recursive implementation.
    println(factorial(30000))
    println(fib(10))

    println(pairs(3))
    println(factors(10))
    println(factors2(10))
    println(factors3(10))

    println(reverse(1::2::3::Nil))
    println(reverseFold(1::2::3::Nil))

    //concatMap (\n -> [n, n * n]) (1 .. 5)
    // >>= is flatMap is bind
    println((1 to 5).toList >>= (n => List(n, n * 2)))
    println((1 to 5).toList flatMap (n => List(n, n * 2)))

  }
}
