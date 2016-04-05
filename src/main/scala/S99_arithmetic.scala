/**
  * Created by Administrator on 2016-3-22.
  */
object S99_arithmetic {

  class S99_Int(nArg: Int) {
    val n = nArg

    /*
      P31 (**) Determine whether a given integer number is prime.
      scala> 7.isPrime
      res0: Boolean = true
      */
    def isPrime: Boolean = n == 2 || List.range(2, n / 2 + 1).forall(i => n % i != 0)

    /*
      P33 (*) Determine whether two positive integer numbers are coprime.
      Two numbers are coprime if their greatest common divisor equals 1.
      scala> 35.isCoprimeTo(64)
      res0: Boolean = true
      */
    def isCoprimeTo(m: S99_Int): Boolean = gcd(m.n, n) == 1

    /*
      P34 (**) Calculate Euler's totient function phi(m).
      Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
      scala> 10.totient
      res0: Int = 4
    */
    def totient: Int = List.range(1, n + 1).map(m => new S99_Int(m)).count(m => m.isCoprimeTo(this))

    /*
      P35 (**) Determine the prime factors of a given positive integer.
      Construct a flat list containing the prime factors in ascending order.
      scala> 315.primeFactors
      res0: List[Int] = List(3, 3, 5, 7)
    */
    def primeFactors: List[Int] = List.range(2, n + 1).map(m => new S99_Int(m)).filter(m => !m.isCoprimeTo(this)) match {
      case Nil => Nil
      case (m :: tail) => m.n :: new S99_Int(this.n / m.n).primeFactors
    }

    /*
      P36 (**) Determine the prime factors of a given positive integer (2).
      Construct a list containing the prime factors and their multiplicity.
      scala> 315.primeFactorMultiplicity
      res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
      Alternately, use a Map for the result.

      scala> 315.primeFactorMultiplicity
      res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
    */
    def primeFactorMultiplicity: Map[Int, Int] = S99_list.encode(primeFactors).map(c => (c._2, c._1)).toMap


    /*
      P37 (**) Calculate Euler's totient function phi(m) (improved).
      See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
      phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...

      Note that ab stands for the bth power of a.
    */
    def pow(b: Int, n: Int): Int =
      if (n == 0) 1
      else if (n % 2 == 0) pow(b * b, n / 2)
      else b * pow(b, n - 1)

    def phi: Int = primeFactorMultiplicity.foldLeft(1) { (h, p) => h * (p._1 - 1) * pow(p._1, p._2 - 1) }


    /*
      P38 (*) Compare the two methods of calculating Euler's totient function.
      Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example.
    */
    val str = "see test(P38) in S99Arithmetic$Test"


    /*
      P40 (**) Goldbach's conjecture.
      Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
      scala> 28.goldbach
      res0: (Int, Int) = (5,23)
    */
    def goldbach: (Int, Int) = {
      val a = listPrimesinRange(Range(2, n)).toStream.dropWhile(a => !new S99_Int(n - a).isPrime).head
      (a, n - a)
    }

  }

  /*
    P32 (**) Determine the greatest common divisor of two positive integer numbers.
    Use Euclid's algorithm.
    scala> gcd(36, 63)
    res0: Int = 9
  */
  def gcd(m: Int, n: Int): Int =
    if (m < n) gcd(n, m)
    else if (m % n == 0) n
    else gcd(n, m % n)

  /*
    P39 (*) A list of prime numbers.
    Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
    scala> listPrimesinRange(7 to 31)
    res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
    */
  def listPrimesinRange(range: Range): List[Int] = range.filter(n => new S99_Int(n).isPrime).toList

  /*
    P41 (**) A list of Goldbach compositions.
    Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
    scala> printGoldbachList(9 to 20)
    10 = 3 + 7
    12 = 5 + 7
    14 = 3 + 11
    16 = 3 + 13
    18 = 5 + 13
    20 = 3 + 17
    In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.

    Example (minimum value of 50 for the primes):

    scala> printGoldbachListLimited(1 to 2000, 50)
    992 = 73 + 919
    1382 = 61 + 1321
    1856 = 67 + 1789
    1928 = 61 + 1867
      */
  def printGoldbachList(range: Range) = for {
    n <- range
    if n > 2 && n % 2 == 0
    (a, b) = new S99_Int(n).goldbach
  } println(n.toString + " = " + a.toString + " + " + b.toString)

  def printGoldbachListLimited(range: Range, limit: Int) = for {
    n <- range
    if n > 2 && n % 2 == 0
    (a, b) = new S99_Int(n).goldbach
    if a > limit && b > limit
  } println(n.toString + " = " + a.toString + " + " + b.toString)
}
