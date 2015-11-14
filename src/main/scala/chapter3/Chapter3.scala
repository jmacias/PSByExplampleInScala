package chapter3

//type definitions
case class Entry (
  firstName: String,
  lastName:  String,
  phone:     String)

object Chapter3 {

  // Type alias and type constructor.
  // type constructors are applied to other types simply by juxtaposition
  type PhoneBook = List[Entry]

  def showEntry(entry: Entry):String =
    s"${entry.lastName}, ${entry.firstName} : ${entry.phone}"

// Some other ways to define a function.
//  val showEntry2: Entry => String =
//    entry => s"${entry.lastName}, ${entry.firstName} : ${entry.phone}"
//   (entry: Entry) => s"${entry.lastName}, ${entry.firstName} : ${entry.phone}"

  val emptyBook: PhoneBook = List.empty

  def insertEntry(entry: Entry, phoneBook: PhoneBook): PhoneBook =
    entry :: phoneBook

  def findEntry(firstName:String, lastName: String, book: PhoneBook): Option[Entry] = {
    val filterEntry: (Entry) => Boolean =
      entry => entry.firstName == firstName && entry.lastName == lastName

    book filter (filterEntry) headOption
  }

  def printEntry(firstName:String, lastName: String, book: PhoneBook): Option[String] =
    findEntry(firstName, lastName, book) map showEntry

  def main(args: Array[String]): Unit = {
    println("Chapter 3.")
    println(printEntry("John", "Foo", emptyBook))

    val john = Entry("John","Smith", "555-555-5555")
    val book1 = insertEntry(john, emptyBook)

    println(printEntry("John", "Smith", book1))

  }

}
