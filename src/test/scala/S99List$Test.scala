import java.util.NoSuchElementException

import org.scalatest.FunSuite

/**
  * Created by Administrator on 2016-3-15.
  */
class S99List$Test extends FunSuite {
  test("P01") {
    assert(S99_list.last(List(1)) == 1)
    assert(S99_list.last(List(1, 2)) == 2)
    intercept[NoSuchElementException] {
      S99_list.last(Nil)
    }
  }

  test("P02") {
    assert(S99_list.penultimate(List(1, 2)) == 1)
    assert(S99_list.penultimate(List(1, 2, 3)) == 2)
    intercept[NoSuchElementException] {
      S99_list.penultimate(Nil)
      S99_list.penultimate(List(1))
    }
  }

  test("P03") {
    assert(S99_list.nth(0, List(1, 2)) == 1)
    assert(S99_list.nth(1, List(1, 2, 3)) == 2)
    assert(S99_list.nth(2, List(1, 2, 3)) == 3)
    intercept[NoSuchElementException] {
      S99_list.nth(-1, List(1, 2, 3))
      S99_list.nth(-1, Nil)
      S99_list.nth(0, Nil)
      S99_list.nth(1, Nil)
      S99_list.nth(3, List(1, 2, 3))
    }
  }

  test("P04") {
    assert(S99_list.length(Nil) == 0)
    assert(S99_list.length(List(1)) == 1)
    assert(S99_list.length(List(1, 2)) == 2)
    assert(S99_list.length(List(1, 2 ,3)) == 3)
  }

  test("P05") {
    assert(S99_list.reverse(Nil) == Nil)
    assert(S99_list.reverse(List(1)) == List(1))
    assert(S99_list.reverse(List(1, 2)) == List(2, 1))
    assert(S99_list.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("P06") {
    assert(S99_list.isPalindrome(Nil))
    assert(S99_list.isPalindrome(List(1)))
    assert(S99_list.isPalindrome(List(1, 2, 1)))
    assert(S99_list.isPalindrome(List(1, 2, 2, 1)))
    assert(!S99_list.isPalindrome(List(1, 2)))
    assert(!S99_list.isPalindrome(List(1, 2, 3)))
  }

  test("P07") {
    assert(S99_list.flatten(Nil) == Nil)
    assert(S99_list.flatten(List(2)) == List(2))
    assert(S99_list.flatten(List(List(1, 1))) == List(1, 1))
    assert(S99_list.flatten(List(List(List(1)))) == List(1))
    assert(S99_list.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  test("P08") {
    assert(S99_list.compress(Nil) == Nil)
    assert(S99_list.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(S99_list.compress(List('a, 'b, 'c, 'a, 'd, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("P09") {
    assert(S99_list.pack(Nil) == Nil)
    assert(S99_list.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(S99_list.pack(List('a, 'b)) == List(List('a), List('b)))
  }

  test("P10") {
    assert(S99_list.encode(Nil) == Nil)
    assert(S99_list.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    assert(S99_list.encode(List('a, 'a, 'b)) == List((2, 'a), (1, 'b)))
  }

  test("P11") {
    assert(S99_list.encodeModified(Nil) == Nil)
    assert(S99_list.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
    assert(S99_list.encodeModified(List('a, 'a, 'b)) == List((2, 'a), 'b))
  }

  test("P12") {
    assert(S99_list.decode(Nil) == Nil)
    assert(S99_list.decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(S99_list.decode(List((2, 'a), (1, 'b))) == List('a, 'a, 'b))
  }

  test("P13") {
    assert(S99_list.encodeDirect(Nil) == Nil)
    assert(S99_list.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    assert(S99_list.encodeDirect(List('a, 'a, 'b)) == List((2, 'a), (1, 'b)))
  }

  test("P14") {
    assert(S99_list.duplicate(Nil) == Nil)
    assert(S99_list.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    assert(S99_list.duplicate(List('a, 'a, 'b)) == List('a, 'a, 'a, 'a, 'b, 'b))
  }

  test("P15") {
    assert(S99_list.duplicateN(0, Nil) == Nil)
    assert(S99_list.duplicateN(0, List('a, 'b, 'c)) == Nil)
    assert(S99_list.duplicateN(1, Nil) == Nil)
    assert(S99_list.duplicateN(1, List('a, 'b, 'c, 'c, 'd)) == List('a, 'b, 'c, 'c, 'd))
    assert(S99_list.duplicateN(2, List('a, 'a, 'b)) == S99_list.duplicate(List('a, 'a, 'b)))
    assert(S99_list.duplicateN(3, List('a, 'b, 'c)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c))
  }

  test("P16") {
    assert(S99_list.drop(0, Nil) ==Nil)
    assert(S99_list.drop(1, List('a, 'b, 'c, 'c, 'd)) == Nil)
    assert(S99_list.drop(2, List('a, 'b, 'c, 'c, 'd)) == List('a, 'c, 'd))
    assert(S99_list.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17") {
    assert(S99_list.split(0, Nil) == (Nil, Nil))
    assert(S99_list.split(0, List('a, 'b, 'c, 'c, 'd)) == (List(), List('a, 'b, 'c, 'c, 'd)))
    assert(S99_list.split(1, List('a)) == (List('a), Nil))
    assert(S99_list.split(1, List('a, 'b, 'c, 'c, 'd)) == (List('a), List('b, 'c, 'c, 'd)))
    assert(S99_list.split(2, List('a, 'b, 'c, 'c, 'd)) == (List('a, 'b), List('c, 'c, 'd)))
    intercept[IndexOutOfBoundsException] {
      S99_list.split(1, Nil)
      S99_list.split(2, List('a))
    }
  }

  test("P18") {
    assert(S99_list.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
    assert(S99_list.slice(3, 7, List('a)) == Nil)
  }

  test("P19") {
    assert(S99_list.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(S99_list.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    assert(S99_list.rotate(-2, Nil) == Nil)
  }

  test("P20") {
    assert(S99_list.removeAt(0, List('a)) == (Nil, 'a))
    assert(S99_list.removeAt(0, List('a, 'b, 'c, 'c, 'd)) == (List('b, 'c, 'c, 'd), 'a))
    assert(S99_list.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
    assert(S99_list.removeAt(2, List('a, 'b, 'c, 'c, 'd)) == (List('a, 'b, 'c, 'd), 'c))
    intercept[IndexOutOfBoundsException] {
      S99_list.removeAt(1, Nil)
      S99_list.removeAt(1, List('a))
      S99_list.removeAt(-2, List('a))
    }
    intercept[NoSuchElementException] {
      S99_list.removeAt(0, Nil)
    }
  }

  test("P21") {
    assert(S99_list.insertAt('a, 0, Nil) == List('a))
    assert(S99_list.insertAt('new, 0, List('a, 'b, 'c, 'c, 'd)) == List('new, 'a, 'b, 'c, 'c, 'd))
    assert(S99_list.insertAt('new, 1, List('a)) == List('a, 'new))
    assert(S99_list.insertAt('new, 1, List('a, 'b, 'c, 'c, 'd)) == List('a, 'new,'b, 'c, 'c, 'd))
    assert(S99_list.insertAt('new, 2, List('a, 'b, 'c, 'c, 'd)) == List('a, 'b, 'new,'c, 'c, 'd))
    intercept[IndexOutOfBoundsException] {
      S99_list.insertAt('new, 1, Nil)
      S99_list.insertAt('new, 2, List('a))
    }
  }

  test("P22") {
    assert(S99_list.range(0, 0) == List(0))
    assert(S99_list.range(0, 1) == List(0, 1))
    assert(S99_list.range(1, 0) == Nil)
    assert(S99_list.range(4, 9) ==  List(4, 5, 6, 7, 8, 9))
  }

  test("P23") {
    assert(S99_list.randomSelect(0, List('a, 'b, 'c, 'c, 'd)).isEmpty)
    assert(S99_list.randomSelect(1, List('a, 'b, 'c, 'd)).length == 1)
    assert(S99_list.randomSelect(2, List('a, 'b, 'c, 'd)).length == 2)
  }

  test("P24") {
    assert(S99_list.lotto(2, 5).length == 2)
    assert(S99_list.lotto(1, 5).length == 1)
    assert(S99_list.lotto(0, 5).isEmpty)
  }

  test("P25") {
    assert(S99_list.randomPermute(List('a, 'b, 'c, 'c, 'd)).length == 5)
    assert(S99_list.randomPermute(List('a, 'b, 'c, 'd)).length == 4)
    assert(S99_list.randomPermute(List('a, 'b, 'c, 'd)).length == 4)
  }

  test("P26") {
    assert(S99_list.combinations(0, List('a, 'b, 'c, 'd)).length == 1)
    assert(S99_list.combinations(1, List('a, 'b, 'c, 'd)).length == 4)
    assert(S99_list.combinations(2, List('a, 'b, 'c, 'd)).length == 6)
    assert(S99_list.combinations(3, List('a, 'b, 'c, 'd)).length == 4)
    assert(S99_list.combinations(4, List('a, 'b, 'c, 'd)).length == 1)
    assert(S99_list.combinations(5, List('a, 'b, 'c, 'd)).isEmpty)
  }

  test("P27") {
    assert(S99_list.group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length == 1260)
    assert(S99_list.group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length == 756)
  }

  test("P28") {
    assert(S99_list.lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) == List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
    assert(S99_list.lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  }
}
