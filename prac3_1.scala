import scala.collection.mutable.ArrayBuffer

@main def main3(): Unit = {
  def reverse_str(str: Array[Char], len: Int, new_str: ArrayBuffer[Char]=ArrayBuffer[Char]()): Array[Char] = {
    if (len == 0)
      new_str.toArray
    else {
      new_str.append(str(len - 1))
      reverse_str(str, len - 1, new_str)
    }
  }
  val originalStr = "hello".toCharArray
  val reversedStr = reverse_str(originalStr, originalStr.length)
  println(reversedStr.mkString)
}