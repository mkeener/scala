package recfun
import common._

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
    if(r == 0 || c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var a = 0
    def loop(acc: Int, c: List[Char]): Boolean = {
      if(acc < 0 || c.isEmpty){
        if(acc == 0) return true
        return false
      } 
      if(c.head == '(') {
        a = a + 1
      } else if (c.head == ')'){
        a = a - 1
      }
      return loop(a, c.tail)	
    }
    return loop(a, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
