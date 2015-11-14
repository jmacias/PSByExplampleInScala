package chapter1

object Chapter1 {

  // Type inference
  def iAmANumber = {
    // return type is inferred.
    val square = (x:Int) =>  x * x
    square(42)
  }

  def main(args: Array[String]): Unit = {
    println("Chapter 1.")

    println("Type Inference.")
    println(iAmANumber)
  }
}
