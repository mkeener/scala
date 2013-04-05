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
    def loop(acc: Int, c: List[Char]): Boolean = {
      if(acc < 0 || c.isEmpty) return (acc == 0)
      val newAcc = 
        if(c.head == '(') acc + 1
        else if (c.head == ')') acc - 1
        else acc
      loop(newAcc, c.tail)
    }
    loop(0, chars)
  }   

  /**
   * Exercise 3
   */
   def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0 || coins.isEmpty) return 0
    var a = 0
    val list = coins.sortWith(_ > _)
    
    //build the list
    def loop(tender: Int, c: List[Int]): Int = {
    	if(tender == 0){
	    	a = a + 1
	    	return a
    	}
    	if(c.isEmpty) return a
    	val heads = tender / c.head
    	for(head <- heads to 0 by -1){
	    	val newtender = tender - (c.head * head)
	    	loop(newtender,c.tail)
    	}
    	return a
    }
    loop(money, list)
  }        
}
