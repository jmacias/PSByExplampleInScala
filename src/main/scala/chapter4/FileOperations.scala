package chapter4

object FileOperations {

  def allFiles(root: Path):List[Path] = root :: (Path.ls(root) flatMap (allFiles _))

  def `allFiles'`(file: Path):List[Path] =  ???

//  allFiles' :: Path -> Array Path
//  allFiles' file = file : do
//  child <- ls file
//    allFiles' child

  def main(args: Array[String]) {
    import scalaz._
    import Scalaz._

    println("Usign Scalaz ShowOps:")
    //Thanks to Scalaz Type class Show we can call println
    Path.root.println

    println("Printing all the files:")
    allFiles(Path.root).map(_.println)
  }

}
