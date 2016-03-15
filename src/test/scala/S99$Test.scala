import java.util.NoSuchElementException

import com.cyl.scala.S99
import org.scalatest.FunSuite

/**
  * Created by Administrator on 2016-3-15.
  */
class S99$Test extends FunSuite {
  test("P01") {
    assert(S99.last(List(1)) == 1)
    assert(S99.last(List(1, 2)) == 2)
    intercept[NoSuchElementException] {
      S99.last(List())
    }
  }

  test("P02") {
    assert(S99.penultimate(List(1, 2)) == 1)
    assert(S99.penultimate(List(1, 2, 3)) == 2)
    intercept[NoSuchElementException] {
      S99.penultimate(List())
      S99.penultimate(List(1))
    }
  }

  test("P03") {
    assert(S99.nth(0, List(1, 2)) == 1)
    assert(S99.nth(1, List(1, 2, 3)) == 2)
    assert(S99.nth(2, List(1, 2, 3)) == 3)
    intercept[NoSuchElementException] {
      S99.nth(-1, List(1, 2, 3))
      S99.nth(-1, List())
      S99.nth(0, List())
      S99.nth(1, List())
      S99.nth(3, List(1, 2, 3))
    }
  }

  test("P04") {
    assert(S99.length(List()) == 0)
    assert(S99.length(List(1)) == 1)
    assert(S99.length(List(1, 2)) == 2)
    assert(S99.length(List(1, 2 ,3)) == 3)
  }

  test("P05") {
    assert(S99.reverse(List()) == List())
    assert(S99.reverse(List(1)) == List(1))
    assert(S99.reverse(List(1, 2)) == List(2, 1))
    assert(S99.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("P06") {
    assert(S99.isPalindrome(List()))
    assert(S99.isPalindrome(List(1)))
    assert(S99.isPalindrome(List(1, 2, 1)))
    assert(S99.isPalindrome(List(1, 2, 2, 1)))
    assert(!S99.isPalindrome(List(1, 2)))
    assert(!S99.isPalindrome(List(1, 2, 3)))
  }

  test("P07") {
    assert(S99.flatten(List()) == List())
    assert(S99.flatten(List(2)) == List(2))
    assert(S99.flatten(List(List(1, 1))) == List(1, 1))
    assert(S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  test("P08") {
    assert(S99.compress(List()) == List())
    assert(S99.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(S99.compress(List('a, 'b, 'c, 'a, 'd, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("P09") {
    assert(S99.pack(List()) == List())
    assert(S99.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(S99.pack(List('a, 'b)) == List(List('a), List('b)))
  }

  test("P10") {
    assert(S99.encode(List()) == List())
    assert(S99.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    assert(S99.encode(List('a, 'a, 'b)) == List((2, 'a), (1, 'b)))
  }

  test("P11") {
    assert(S99.encodeModified(List()) == List())
    assert(S99.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
    assert(S99.encodeModified(List('a, 'a, 'b)) == List((2, 'a), 'b))
  }

  test("P12") {
    assert(S99.decode(List()) == List())
    assert(S99.decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(S99.decode(List((2, 'a), (1, 'b))) == List('a, 'a, 'b))
  }

  test("P13") {
    assert(S99.encodeDirect(List()) == List())
    assert(S99.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    assert(S99.encodeDirect(List('a, 'a, 'b)) == List((2, 'a), (1, 'b)))
  }

  test("P14") {
    assert(S99.duplicate(List()) == List())
    assert(S99.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    assert(S99.duplicate(List('a, 'a, 'b)) == List('a, 'a, 'a, 'a, 'b, 'b))
  }

  test("P15") {
    assert(S99.duplicateN(0, List()) == List())
    assert(S99.duplicateN(1, List('a, 'b, 'c, 'c, 'd)) == List('a, 'b, 'c, 'c, 'd))
    assert(S99.duplicateN(2, List('a, 'a, 'b)) == S99.duplicate(List('a, 'a, 'b)))
    assert(S99.duplicateN(3, List('a, 'b, 'c)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c))
  }

  test("P16") {
    assert(S99.drop(0, List()) == List())
    assert(S99.drop(1, List('a, 'b, 'c, 'c, 'd)) == List())
    assert(S99.drop(2, List('a, 'b, 'c, 'c, 'd)) == List('a, 'c, 'd))
    assert(S99.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17") {
    assert(S99.split(0, List()) == (List(), List()))
    assert(S99.split(0, List('a, 'b, 'c, 'c, 'd)) == (List(), List('a, 'b, 'c, 'c, 'd)))
    assert(S99.split(1, List('a)) == (List('a), List()))
    assert(S99.split(1, List('a, 'b, 'c, 'c, 'd)) == (List('a), List('b, 'c, 'c, 'd)))
    assert(S99.split(2, List('a, 'b, 'c, 'c, 'd)) == (List('a, 'b), List('c, 'c, 'd)))
    intercept[IndexOutOfBoundsException] {
      S99.split(1, List())
      S99.split(2, List('a))
    }
  }

  test("P18") {
    assert(S99.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }
}
