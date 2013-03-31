package week2

object exercise {
  def factorial(n: Int): Int = {
  	def loop(acc: Int, n: Int): Int = {
  		if(n == 0) acc
  		else loop(acc * n, n - 1)
  	}
  	loop(1, n)
  }                                               //> factorial: (n: Int)Int
  factorial(4)                                    //> res0: Int = 24
  factorial(5)                                    //> res1: Int = 120
  factorial(10)                                   //> res2: Int = 3628800
  factorial(1)                                    //> res3: Int = 1
}