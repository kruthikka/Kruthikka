import scala.collection.mutable.Set
import scala.io.StdIn.{readLine, readInt}

object library_management {
  
  case class Book(title: String, author: String, isbn: String)

  // Initially declared three books
  val book1 = Book("Scala Programming Basics", "John Doe", "1234567890")
  val book2 = Book("Data Structures in Scala", "Jane Smith", "0987654321")
  val book3 = Book("Functional Programming", "Chris Brown", "5678901234")

  val books: Set[Book] = Set(book1, book2, book3)

  def main(args: Array[String]): Unit = {
    getBook()
    findBookIsbn("1234567890")
    searchBooksByTitle("Scala Programming Basics")
    printBooks(books)
    removeBook("0987654321")
    printBooks(books)
  }

  // Add a book
  def getBook(): Unit = {
    val bookName = readLine("Enter the name of the book: ")
    val bookAuthor = readLine("Enter the author: ")
    val bookIsbn = readLine("Enter the ISBN of the book: ")
    val newBook = Book(bookName, bookAuthor, bookIsbn)
    books += newBook
    println(s"Book titled '$bookName' added successfully.")
  }

  // Find book by ISBN
  def findBookIsbn(bookIsbn: String): Unit = {
    val foundbook: Option[Book] = books.find(_.isbn == bookIsbn)
    foundbook match {
      case Some(book) =>
        println(s"Found book: ${book.title} by ${book.author}, ISBN: ${book.isbn}")
      case None =>
        println(s"No book found with ISBN: $bookIsbn")
    }
  }

  // Search books by title
  def searchBooksByTitle(title: String): Unit = {
    val foundBooks = books.filter(_.title.equalsIgnoreCase(title))

    if (foundBooks.nonEmpty) {
      println(s"Found books matching '$title':")
      foundBooks.foreach(book => println(s"Title: ${book.title}, Author: ${book.author}, ISBN: ${book.isbn}"))
    } else {
      println(s"No books found matching '$title'.")
    }
  }

  // Print all books
  def printBooks(bookSet: Set[Book]): Unit = {
    if (bookSet.nonEmpty) {
      bookSet.foreach(book => println(s"Title: ${book.title} _ Author: ${book.author} _ ISBN: ${book.isbn}"))
    } else {
      println("No books found.")
    }
  }

  // Remove book by ISBN
  def removeBook(isbn: String): Unit = {
    books.find(_.isbn == isbn) match {
      case Some(book) =>
        books -= book
        println(s"Book with ISBN $isbn removed.")
      case None =>
        println(s"No book found with ISBN: $isbn")
    }
  }
}