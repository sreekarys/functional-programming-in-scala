package recfun

import scala.collection.mutable

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def pascalRecursive(r: Int): List[Int] = {
      if(r == 0)
        List(1)
      else
        getNextPascalRow(pascalRecursive(r-1))
    }

    def getNextPascalRow(pascalRow: List[Int]): List[Int] = {
      List(1).appendedAll(getNextPascalElements(pascalRow)).appended(1)
    }

    def getNextPascalElements(pascalRow: List[Int]): List[Int] = {
      if(pascalRow.tail.isEmpty) List()
      else {
        val tail = pascalRow.tail
        List(tail.head + pascalRow.head).appendedAll(getNextPascalElements(tail))
      }
    }

    if(r < 0 || c < 0 || c > r)
      throw new IllegalArgumentException("Either r or c is/are invalid!!!")
    pascalRecursive(r)(c)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balance(remaining: Int, chars: List[Char]): Boolean = {
      if(chars.isEmpty)
        remaining == 0
      else if(remaining < 0)
        false
      else if(chars.head == '(')
        balance(remaining+1, chars.tail)
      else if(chars.head == ')')
        balance(remaining-1, chars.tail)
      else
        balance(remaining, chars.tail)
    }

    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeRecursive(money: Int, n: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0) 0
      else if(money > 0 && n <= 0) 0
      else countChangeRecursive(money, n-1, coins) + countChangeRecursive(money - coins(n-1), n, coins)
    }

    if(coins.isEmpty) 0
    else countChangeRecursive(money, coins.length, coins)
  }
}
