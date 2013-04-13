package funsets

object testset {
	/**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = ((x: Int) => x == elem)
                                                  //> singletonSet: (elem: Int)Int => Boolean
	val s1 = singletonSet(1)                  //> s1  : Int => Boolean = <function1>
	val s2 = singletonSet(2)                  //> s2  : Int => Boolean = <function1>
	val s3 = singletonSet(3)                  //> s3  : Int => Boolean = <function1>

	
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = ((x: Int) => contains(s, x) || contains(t, x))
                                                  //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
	contains(s1, 2)                           //> res0: Boolean = false
	contains(union(s1, s2), 2)                //> res1: Boolean = true
	contains(union(s1, s2), 3)                //> res2: Boolean = false
	val s4 = union(s1, s2)                    //> s4  : Int => Boolean = <function1>
	val s5 = union(s3, s4)                    //> s5  : Int => Boolean = <function1>
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = ((x: Int) => contains(s, x) && contains(t, x))
                                                  //> intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = ((x: Int) => contains(s, x) && !contains(t, x))
                                                  //> diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)
                                                  //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000                                //> bound  : Int = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !contains(intersect(s, p), a)) false
      else iter(a+1)
    }
    iter(-1000)
  }                                               //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean

	forall(s4, ((x: Int) => x > 0))           //> res3: Boolean = true
	

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (contains(intersect(s, p), a)) true
      else iter(a+1)
    }
    iter(-1000)
  }                                               //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean

	exists(s4, ((x: Int) => x == 1))          //> res4: Boolean = true

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = ???       //> map: (s: Int => Boolean, f: Int => Int)Int => Boolean

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Int => Boolean)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Int => Boolean)Unit
}