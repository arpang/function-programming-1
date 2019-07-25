package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      return 1
    }
    return pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def internal(chars: List[Char], count: Int): Boolean = {
      if (count < 0) return false

      if (chars.isEmpty) return count == 0

      val head = chars.head;

      if (head == '(') {
        return internal(chars.tail, count + 1)
      } else if (head == ')') {
        return internal(chars.tail, count - 1)
      } else {
        return internal(chars.tail, count)
      }
    }

    return internal(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) return 1

    if (coins.isEmpty) return 0

    val headCoin = coins.head
    var result = 0
    var moneyLeft = money

    while (moneyLeft >= 0) {
      result += countChange(moneyLeft, coins.tail)
      moneyLeft -= headCoin
    }
    return result
  }
}
