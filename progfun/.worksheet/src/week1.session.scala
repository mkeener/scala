package week1

object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(39); val res$0 = 
  1 + 2;System.out.println("""res0: Int(3) = """ + $show(res$0));$skip(44); 
  def abs(x: Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(330); 

  def sqrt(x: Double) = {
  
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess), x)

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(11); val res$1 = 

  sqrt(2);System.out.println("""res1: Double = """ + $show(res$1));$skip(10); val res$2 = 
  sqrt(4);System.out.println("""res2: Double = """ + $show(res$2));$skip(11); val res$3 = 
  sqrt(64);System.out.println("""res3: Double = """ + $show(res$3));$skip(13); val res$4 = 
  sqrt(1e-6);System.out.println("""res4: Double = """ + $show(res$4));$skip(13); val res$5 = 
  sqrt(1e60);System.out.println("""res5: Double = """ + $show(res$5));$skip(329); 
  
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
  };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(49); val res$6 = 
  
	balance("(if (zero? x) max (/ 1 x))".toList);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(85); val res$7 = 
	balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(23); val res$8 = 
	balance(":-)".toList);System.out.println("""res8: Boolean = """ + $show(res$8));$skip(24); val res$9 = 
	balance("())(".toList);System.out.println("""res9: Boolean = """ + $show(res$9))}
}
