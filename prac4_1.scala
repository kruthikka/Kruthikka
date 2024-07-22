import scala.collection.mutable.ArrayBuffer

@main def main3(): Unit = {
  var Inventory_item: Array[String] = Array("Rice", "Sugar", "Flour")
  var Inventory_quantity: Array[Int] = Array(5, 12, 8)

  def restock(item: String, quantity: Int): Unit = {
    var found = false
    for (i <- Inventory_item.indices) {
      if (item == Inventory_item(i)) {
        Inventory_quantity(i) += quantity
        found = true
      }
    }
    if (!found) {
      println("Item is not found")
    }
  }

  def sellitem(item: String, quantity: Int): Unit = {
    var found = false
    for (i <- Inventory_item.indices) {
      if (item == Inventory_item(i)) {
        Inventory_quantity(i) -= quantity
        found = true
      }
    }
    if (!found) {
      println("Item is not found")
    }
  }

  def displayinventory(): Unit = {
    for (i <- Inventory_item.indices) {
      println(s"Item name: ${Inventory_item(i)}, quantity: ${Inventory_quantity(i)} Kg")
    }
  }
  
  restock("Rice", 3)
  sellitem("Sugar", 2)
  displayinventory()
}