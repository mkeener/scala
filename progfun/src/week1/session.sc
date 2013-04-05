package week1

object session {
  1 + 2                                           //> res0: Int(3) = 3
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {
  
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess), x)

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res1: Double = 1.4142156862745097
  sqrt(4)                                         //> res2: Double = 2.0000000929222947
  sqrt(64)                                        //> res3: Double = 8.000001655289593
  sqrt(1e-6)                                      //> res4: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res5: Double = 1.0000000031080746E30
  
  def balance(chars: List[Char]): Boolean = {
    var a = 0
    def loop(acc: Int, c: List[Char]): Boolean = {
      if(acc < 0 || c.isEmpty) return (acc == 0)
      val newAcc =
        if(c.head == '(') acc + 1
        else if (c.head == ')') acc - 1
        else acc
      loop(newAcc, c.tail)
    }
    loop(0, chars)
  }                                               //> balance: (chars: List[Char])Boolean
  
	balance("(if (zero? x) max (/ 1 x))".toList)
                                                  //> res6: Boolean = true
	balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
                                                  //> res7: Boolean = true
	balance(":-)".toList)                     //> res8: Boolean = false
	balance("())(".toList)                    //> res9: Boolean = false
}