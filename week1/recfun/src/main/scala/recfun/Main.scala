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
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def internal(chars: List[Char], count: Int): Boolean = {
      if (count < 0) false

      else if (chars.isEmpty) count == 0

      else {
        val head = chars.head
        if (head == '(') {
          internal(chars.tail, count + 1)
        } else if (head == ')') {
          internal(chars.tail, count - 1)
        } else {
          internal(chars.tail, count)
        }
      }
    }

    internal(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
      val headCoin = coins.head
      var result = 0
      var moneyLeft = money
      while (moneyLeft >= 0) {
        result += countChange(moneyLeft, coins.tail)
        moneyLeft -= headCoin
      }
      result
    }
  }
}
