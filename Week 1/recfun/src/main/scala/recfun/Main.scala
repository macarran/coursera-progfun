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
    if (c == 0 && r == 0) 1 // base condition
    else if (c < 0 || r < 0) 0 // out of bounds elements
    else pascal(c - 1, r - 1) + pascal(c, r - 1) // ok elements
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec //make it tail recursive... last thing done in function is recursive call
    def isBalanced(open: Int, chars: List[Char]): Boolean = {
      if (open < 0) false // check for ') ()(())'
      else if (chars.isEmpty) open == 0 // finished list without going negative, check balanced
      else {
        if (chars.head == '(') isBalanced(open + 1, chars.tail) // add open to balance
        else if (chars.head == ')') isBalanced(open - 1, chars.tail) // substract open to balance
        else isBalanced(open, chars.tail) // keep searching w/o changes
      }
    }

    isBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	//helper function
    def sums(total: Int, coinsLeft: List[Int]): Int = {
      if (total == money) 1 // if match, return valid combination
      else if (total > money || coinsLeft.isEmpty) 0 	// (1)
      else sums(total + coinsLeft.head, coinsLeft) +	// (2)      	
      	sums(total, coinsLeft.tail)						// (3)
      // (1) if surpassed total or no more coins left, end combination
      // (2) keep on trying with current value and (3)
      // (3) try keeping same total but using next coins in list (combinatory)
    }

    if(coins.isEmpty) 0 // can't give change w/o coins
    else sums(0, coins)
  }
}
