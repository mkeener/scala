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
  
  //Tail-recursive sum function from Video 1.
  
  def sum(f: Int => Int, a: Int, b: Int): Int ={
  	def loop(a: Int, acc: Int): Int = {
  		if(a > b) acc
  		else loop(a + 1, f(a) + acc)
  	}
  	loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  
  sum(x => x * x, 3, 5)                           //> res4: Int = 50

	//Currying examples.
	def sum2(f: Int => Int)(a: Int, b: Int): Int = {
			if(a > b) 0 else f(a) + sum2(f)(a + 1, b)
	}                                         //> sum2: (f: Int => Int)(a: Int, b: Int)Int
	
	sum2(x => x*x)(3, 5)                      //> res5: Int = 50

	//product function
	def product(f: Int => Int)(a: Int, b: Int): Int ={
		if(a > b) 1 else f(a) * product(f)(a + 1, b)
	}                                         //> product: (f: Int => Int)(a: Int, b: Int)Int
	//Product of squares
	product(x => x * x)(3, 4)                 //> res6: Int = 144
	
	//Factorial in terms of product
	def fact(n: Int) = product(x => x)(1, n)  //> fact: (n: Int)Int
	fact(5)                                   //> res7: Int = 120
	
	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int ={
		if(a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
	}                                         //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int
	def productReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a, b)
                                                  //> productReduce: (f: Int => Int)(a: Int, b: Int)Int
	//Product of squares
	productReduce(x => x*x)(3,4)              //> res8: Int = 144
	
	def factReduce(n: Int) = productReduce(x => x)(1, n)
                                                  //> factReduce: (n: Int)Int
	factReduce(5)                             //> res9: Int = 120
	
	def sumReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
                                                  //> sumReduce: (f: Int => Int)(a: Int, b: Int)Int
	//Sum of ints.
	sumReduce(x => x)(0, 5)                   //> res10: Int = 15
}