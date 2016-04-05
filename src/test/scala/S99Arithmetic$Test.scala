import S99_arithmetic.S99_Int
import org.scalatest.FunSuite

/**
  * Created by Administrator on 2016-3-22.
  */
class S99Arithmetic$Test extends FunSuite {
  test("P31") {
    assert(new S99_Int(2).isPrime)
    assert(new S99_Int(5).isPrime)
    assert(new S99_Int(7).isPrime)
    assert(new S99_Int(13).isPrime)
    assert(new S99_Int(23).isPrime)
    assert(!new S99_Int(4).isPrime)
    assert(!new S99_Int(9).isPrime)
  }

  test("P32") {
    assert(S99_arithmetic.gcd(3, 2) == 1)
    assert(S99_arithmetic.gcd(35, 64) == 1)
    assert(S99_arithmetic.gcd(36, 63) == 9)
  }

  test("P33") {
    assert(new S99_Int(35).isCoprimeTo(new S99_Int(64)))
    assert(new S99_Int(3).isCoprimeTo(new S99_Int(2)))
    assert(!new S99_Int(36).isCoprimeTo(new S99_Int(63)))
  }

  test("P34") {
    assert(new S99_Int(10).totient == 4)
    assert(new S99_Int(5).totient == 4)
    assert(new S99_Int(6).totient == 2)
    assert(new S99_Int(7).totient == 6)
    assert(new S99_Int(13).totient == 12)
  }

  test("P35") {
    assert(new S99_Int(315).primeFactors == List(3, 3, 5, 7))
    assert(new S99_Int(4).primeFactors == List(2, 2))
    assert(new S99_Int(10).primeFactors == List(2, 5))
    assert(new S99_Int(2).primeFactors == List(2))
    assert(new S99_Int(1).primeFactors == Nil)
  }

  test("P36") {
    assert(new S99_Int(315).primeFactorMultiplicity == Map(3 -> 2, 5 -> 1, 7 -> 1))
    assert(new S99_Int(4).primeFactorMultiplicity == Map(2 -> 2))
    assert(new S99_Int(10).primeFactorMultiplicity == Map(2 -> 1, 5 -> 1))
    assert(new S99_Int(2).primeFactorMultiplicity == Map(2 -> 1))
    assert(new S99_Int(1).primeFactorMultiplicity.isEmpty)
  }

  test("P37") {
    assert(new S99_Int(10).phi == 4)
    assert(new S99_Int(5).phi == 4)
    assert(new S99_Int(6).phi == 2)
    assert(new S99_Int(7).phi == 6)
    assert(new S99_Int(13).phi == 12)
  }

  test("P38") {
    assert(new S99_Int(10).totient == new S99_Int(10).phi)
    assert(new S99_Int(2).totient == new S99_Int(2).phi)
    assert(new S99_Int(100).totient == new S99_Int(100).phi)
    assert(new S99_Int(248).totient == new S99_Int(248).phi)
    assert(new S99_Int(512).totient == new S99_Int(512).phi)
  }

  test("P39") {
    assert(S99_arithmetic.listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
    assert(S99_arithmetic.listPrimesinRange(2 to 10) == List(2, 3, 5, 7))
  }

  test("P40") {
    assert(List((3, 7), (5, 5)) contains new S99_Int(10).goldbach)
    assert(List((5, 23), (11, 17)) contains new S99_Int(28).goldbach)
    assert(List((2, 2)) contains new S99_Int(4).goldbach)
    assert(List((3, 11), (7, 7)) contains new S99_Int(14).goldbach)
  }

  test("P41") {

  }
}
