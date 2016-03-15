package com.cyl.scala

/**
  * @author cyl
  */
object S99 {
  /*
p01 (*) Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
*/
  def last[T](list: List[T]): T = list match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }


  /*
p02 (*) Find the last but one element of a list.
  Example:
  scala> penultimate(List(1, 1, 2, 3, 5, 8))
  res0: Int = 5
*/
  def penultimate[T](list: List[T]): T = list match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }


  /*
p03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:
scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
*/
  def nth[T](n: Int, list: List[T]): T = (n, list) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, h :: tail) => h
    case (a, _ :: tail) => nth(a - 1, tail)
  }


  /*
p04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
*/
//  def length[T](list: List[T]): Int = list match {
//    case Nil => 0
//    case _ :: tail => 1 + length(tail)
//  }
  def length[T](list : List[T]) : Int = list.foldLeft(0) { (count, _) => count + 1 }


  /*
p05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/
  //  def reverse[T](list : List[T]) : List[T] = list match {
  //    case Nil => Nil
  //    case h :: tail => reverse(tail) ::: List(h)
  //  }
  //  def reverse[T](list : List[T]) : List[T] = {
  //    def _reverse[T](list : List[T], res : List[T]) : List[T] = list match {
  //      case Nil => res
  //      case h :: tail => _reverse(tail, h :: res)
  //    }
  //    _reverse(list, Nil)
  //  }
  def reverse[T](list: List[T]): List[T] = list.foldLeft(List[T]()) { (head, tail) => tail :: head }


  /*
p06 (*) Find out whether a list is a palindrome.
Example:
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true*/
  def isPalindrome[T](list: List[T]): Boolean = list == reverse(list)


  /*
P07 (**) Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
*/
  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => List(head) ::: flatten(tail)
  }


  /*
p08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:
scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
*/
  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => head :: compress(tail.dropWhile(_ == head))
  }


  /*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:
scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
*/
  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case _ => {
      val (first, second) = list.span {
        _ == list.head
      }
      first :: pack(second)
    }
  }


  /*
P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:
scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/
  def encode[T](list: List[T]): List[(Int, T)] = pack(list) map { x => (x.length, x.head) }


  /*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:
scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
  def encodeModified[T](list: List[T]): List[Any] = pack(list) map { x => if (x.length > 1) (x.length, x.head) else x.head }


  /*
P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:
scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
*/
  def decode[T](list: List[(Int, T)]) = list flatMap { x => for (i <- List.range(0, x._1)) yield x._2 }


  /*
P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
Example:

scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case _ => {
      val (first, second) = list.span {
        _ == list.head
      }
      (first.length, first.head) :: encodeDirect(second)
    }
  }


  /*
P14 (*) Duplicate the elements of a list.
Example:
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
*/
  def duplicate[T](list: List[T]): List[T] = list flatMap { x => List(x, x) }


  /*
P15 (**) Duplicate the elements of a list a given number of times.
Example:
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
*/
  def duplicateN[T](n: Int, list: List[T]): List[T] = list flatMap { x => for (i <- List.range(0, n)) yield x }


  /*
P16 (**) Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
*/
  def drop[T](n: Int, list: List[T]): List[T] = {
    def _drop[T](n: Int, list: List[T], i: Int, res: List[T]): List[T] = (list, i) match {
      case (Nil, _) => res
      case (_ :: tail, 1) => _drop(n, tail, n, res)
      case (ch :: tail, _) => _drop(n, tail, i - 1, res ::: List(ch))
    }
    _drop(n, list, n, Nil)
  }


  /*
P17 (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:
scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
*/
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {
      def _split[T](n: Int, head: List[T], tail : List[T]): (List[T], List[T]) = (n, tail) match {
        case (0, _) => (head, tail)
        case (_, Nil) => throw new IndexOutOfBoundsException
        case (_, h :: t) => _split(n - 1, head ::: List(h), t)
    }
    _split(n, List(), list)
  }


  /*
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:
scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
*/
  def slice[T](from: Int, to: Int, list: List[T]): List[T] =
    if (list.isEmpty || from < 0 || from > to) Nil
    else list take(to) drop(from)


  /*
 *
 * SICP
 *
 */
  //2.4
  //  def cons(p : Int, q : Int) = (m : (Int, Int) => Int) => m(p, q)
  //
  //  def car(z : ((Int, Int) => Int) => Int) = z((p : Int, q : Int) => q)
  //
  //  def cdr(z : ((Int, Int) => Int) => Int) = z((p : Int, q : Int) => p)
  //
  //  //2.6
  //  def zero() = (f : Any) => (x : Any) => x
  //
  //  def add_one(n : Any => Any => Any => Any) = (f : (Any, Any) => Any) => (x : Any) => f(n(f), x)

}