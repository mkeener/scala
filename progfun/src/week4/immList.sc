package week4

object immList {

	def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week4.Cons[T]
	singleton(1)                              //> res0: week4.Cons[Int] = week4.Cons@70d04b77
	singleton(true)                           //> res1: week4.Cons[Boolean] = week4.Cons@7b0aab7d
	singleton[Int](5)                         //> res2: week4.Cons[Int] = week4.Cons@7980daa6
	
	def nth[T](n: Int, list: List[T]): T = {
		if(list.isEmpty) throw new IndexOutOfBoundsException
		if(n == 0) list.head
		else nth(n - 1, list.tail)
	}                                         //> nth: [T](n: Int, list: week4.List[T])T
	
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week4.Cons[Int] = week4.Cons@7a93069b
	
	nth(2, list)                              //> res3: Int = 3
	//nth(-1, list)
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head: Nothing = throw new NoSuchElementException("Nil.head")
	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}