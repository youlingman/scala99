import scala.collection.immutable.SortedMap

/**
  * Created by Administrator on 2016-4-11.
  */
object S99_logic {

  /* P46 (**) Truth tables for logical expressions.
    Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
    scala> and(true, true)
    res0: Boolean = true

    scala> xor(true. true)
    res1: Boolean = false
    A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

    Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

    scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    A     B     result
    true  true  true
    true  false true
    false true  false
    false false false
  */
  def not(a: Boolean): Boolean = !a

  def and(a: Boolean, b: Boolean): Boolean = a && b

  def or(a: Boolean, b: Boolean): Boolean = a || b

  def nand(a: Boolean, b: Boolean): Boolean = !and(a, b)

  def nor(a: Boolean, b: Boolean): Boolean = !or(a, b)

  def xor(a: Boolean, b: Boolean): Boolean = a != b

  def impl(a: Boolean, b: Boolean): Boolean = !a || b

  def equ(a: Boolean, b: Boolean): Boolean = a == b

  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    println("A\tB\tresult")
    for {
      a <- List(true, false)
      b <- List(true, false)
    } println(a + "\t" + b + "\t" + f(a, b))
  }

  class S99_Boolean(a: Boolean) {

    def !: = !a

    def and(b: Boolean): Boolean = a && b

    def or(b: Boolean): Boolean = a || b

    def nand(b: Boolean): Boolean = !and(b)

    def nor(b: Boolean): Boolean = !or(b)

    def xor(b: Boolean): Boolean = a != b

    def impl(b: Boolean): Boolean = !a || b

    def equ(b: Boolean): Boolean = a == b
  }

  def gray(n: Int): List[String] = n match {
    case 0 => List("")
    case _ =>
      val p = gray(n - 1)
      p.map{ "0" + _ }  ::: p.reverse.map{ "1" + _ }
  }

  def huffman(l: List[(String, Int)]): List[(String, String)] = {
    val m = l.map(_.swap).map{ case (n, c) => (n, List((c, ""))) }.sortBy(_._1)
    def _huffman(l: List[(Int, List[(String, String)])]) : List[(String, String)] = {
      if (l.length == 1) l.head._2
      else {
        val t_1 = l.head
        val t_2 = l.tail.head
        _huffman((t_1._1 + t_2._1, t_1._2.map { case (c, s) => (c, "0" + s) } ::: t_2._2.map { case (c, s) => (c, "1" + s)}) :: l.tail.tail sortBy(_._1))
      }
    }
    _huffman(m)
  }


}
