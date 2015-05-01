package recfun
import common._
import scala.annotation.tailrec

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
    c match {
      case  0  => 1
      case `r` => 1
      case  _  => pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def balanceLoop(chars: List[Char], unmatched: Int): Boolean = {
      if (chars.isEmpty) unmatched == 0
      else {
        val newUnmatched = chars.head match {
          case '(' => unmatched + 1
          case ')' => unmatched - 1
          case  _  => unmatched
        }
        balanceLoop(chars.tail, newUnmatched.abs)
      }
    }

    balanceLoop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // If we got the amount down to 0, we have correct change
    if (money == 0) 1
    // If we went under 0, or ran out of coin, we do not have correct change
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
