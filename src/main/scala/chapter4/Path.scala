package chapter4

import scalaz.Show

trait Path
case class Directory(name: String, files: List[Path]) extends Path
case class File(name: String, size: Int) extends Path

object Path {

  implicit val pathShow = Show.shows[Path] { path =>
    fileName(path)
  }

  def fileName(file: Path): String = file match {
    case Directory(name, _) => name
    case File(name,_)       => name
  }

  def isDirectory(file: Path): Boolean = file match {
    case Directory(_, _) => true
    case _               => false
  }

  def ls(file: Path): List[Path] = file match {
    case Directory(_, fs) => fs
    case _               => List.empty
  }

  def size(file: Path): Option[Int] = file match {
    case File(_, bytes) => Some(bytes)
    case _              => None
  }

  val root: Path =
    Directory("/",
      List(
        Directory("/bin/",
          List(
            File("/bin/cp", 24800),
            File("/bin/ls", 34700),
            File("/bin/mv", 20200))),
        Directory("/etc/",
          List(
            File("/etc/hosts", 300))),
        Directory("/home/",
          List(
            Directory("/home/user/",
              List(
                File("/home/user/todo.txt", 1020),
                Directory("/home/user/code/",
                  List(
                    Directory("/home/user/code/js/",
                      List(
                        File("/home/user/code/js/test.js", 40000))),
                    Directory("/home/user/code/haskell/",
                      List(
                        File("/home/user/code/haskell/test.hs", 5000)))))))))))

}


