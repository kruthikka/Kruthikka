object InventorySystem extends App {
  
  // Define the product case class
  case class Product(name: String, quantity: Int, price: Double)
  
  // Define inventory1 and inventory2
  val inventory1: Map[Int, Product] = Map(
    101 -> Product("Product1", 10, 15.5),
    102 -> Product("Product2", 5, 25.0),
    103 -> Product("Product3", 20, 30.0)
  )
  
  val inventory2: Map[Int, Product] = Map(
    102 -> Product("Product2", 10, 20.0),
    104 -> Product("Product4", 7, 50.0)
  )
  
  // I. Retrieve all product names from inventory1
  def getAllProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }
  
  // II. Calculate the total value of all products in inventory1
  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(p => p.quantity * p.price).sum
  }
  
  // III. Check if inventory1 is empty
  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }
  
  // IV. Merge inventory1 and inventory2
  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    inventory2.foldLeft(inventory1) {
      case (acc, (id, product)) =>
        acc.get(id) match {
          case Some(existingProduct) =>
            acc.updated(id, Product(
              name = existingProduct.name,
              quantity = existingProduct.quantity + product.quantity,
              price = math.max(existingProduct.price, product.price)
            ))
          case None =>
            acc + (id -> product)
        }
    }
  }
  
  // V. Check if a product with a specific ID (e.g., 102) exists and print its details
  def checkProductExists(inventory: Map[Int, Product], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some(product) =>
        println("Item exists")
        println(s"Product details: $product")
      case None =>
        println("Item does not exist")
    }
  }
  
  // Test the functions
  println("I. All product names from inventory1: " + getAllProductNames(inventory1))
  
  println("II. Total value of all products in inventory1: " + calculateTotalValue(inventory1))
  
  println("III. Is inventory1 empty? " + isInventoryEmpty(inventory1))
  
  println("IV. Merged inventory: " + mergeInventories(inventory1, inventory2))
  
   println("V. Does product with ID 102 exist in inventory1?")
  checkProductExists(inventory1, 102)
}
