package week2

object classAndObj {
	val x = new Rational(1, 3)                //> x  : week2.Rational = 1/3
	x.numer                                   //> res0: Int = 1
	x.denom                                   //> res1: Int = 3
	
	val y = new Rational(5, 7)                //> y  : week2.Rational = 5/7
	val z = new Rational(3, 2)                //> z  : week2.Rational = 3/2
	
	x + y                                     //> res2: week2.Rational = 22/21
	-x                                        //> res3: week2.Rational = 1/-3
	
	x - y - z                                 //> res4: week2.Rational = -79/42
	
	y + y                                     //> res5: week2.Rational = 10/7
	
	x < y                                     //> res6: Boolean = true
	x.max(y)                                  //> res7: week2.Rational = 5/7

	new Rational(2)                           //> res8: week2.Rational = 2/1
}



class Rational(x: Int, y: Int) {
	require(y != 0, "denomintaor must be non-zero")
	//require(y > 0, "denominator must be positive")
	
	//Secondary constructor example.
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
	val numer = x
	val denom = y
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if(this < that) that else this
	
	def +(that: Rational) =
		new Rational(
		numer * that.denom + that.numer * denom,
		denom * that.denom)
		
	def unary_- : Rational = new Rational(-numer, denom)
	
	def -(that: Rational) = this + -that
		
		
	override def toString = {
		val g = gcd(x, y)
		numer / g + "/" + denom / g
	}
}