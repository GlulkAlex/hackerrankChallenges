package fibonacciFindingEasy

import java.math.BigInteger

import scala.annotation.tailrec

/**
 * Created by gluk-alex on 8/28/15.
 */
/*
>>>Problem Statement:
You're given three numbers:
'A', 'B', and 'N', and
all you have to do is
to find the number 'FNth' where:
F0 = A
F1 = B
? sum of two previous ?
Fi = Fi−1 + Fi−2 for i≥2
As the number can be very large, output it `modulo` 10^9 + 7.

Consider the following link:
http://en.wikipedia.org/wiki/Fibonacci_number#Matrix_form

>>Input Format:
First line contains
a single integer 'T' - the number of tests.
'T' `lines` follow,
each containing three integers: 'A', 'B' and 'N'.

Constraints:
1≤T≤1000
1≤A, B, N≤109

>Output Format
For each test case output a single integer − 'FN'.
>Sample Input
8
2 3 1
9 1 7
9 8 3
2 4 9
1 7 2
1 8 1
4 3 1
3 7 5
>Sample Output
3
85
25
178
8
8
3
44
>Explanation:
First test case is obvious.
Let's look through the second one:
F0=9
F1=1
F2=1+9=10
F3=10+1=11
F4=11+10=21
F5=21+11=32
F6=32+21=53
F7=53+32=85
 */
/*
It is interesting to note that
the n-th Fibonacci number grows so fast that
F47 exceeds the 32-bit signed integer range.

The fastest way to accurately compute `Fibonacci numbers` is
by using a `matrix-exponentiation` method.
We need to
calculate MN to
calculate the Nth `fibonacci number`.
We can
calculate it in O(log(N)) using `fast exponentiation`.
 */
/*
>>>Using `Matrix Exponentiation`:
//Calculating A^p in O(log(P))
Matrix_pow ( Matrix A,int p )
    if(p=1)
        return A
    if(p%2=1)
        return A*Matrix_pow(A,p-1)

    Matrix B = Matrix_pow(a,p/2);
    return B * B

fibonacci(n)
    if(n=0)
        return 0;
    if(n=1)
        return 1;
    Matrix M[2][2]={{1,1},{1,0}}
    Matrix res=matrix_pow(M,n-1);
    return res[0][0];
//Time Complexity = O(log(n))
 */
/*
also from:
http://www.nayuki.io/page/fast-fibonacci-algorithms
 */
object Solution {
  def fibFrom(a: Int, b: Int):
  Stream[Int] =
  /*Since it uses '#::',
  though,
  the right-hand side is not `evaluated` until it is `requested`.
  * */
    a #:: fibFrom(b, a + b)

  //scala.annotation
  @tailrec
  def fibFrom2(
                /*sequence start*/
                a: Int,
                b: Int,
                /*resultAccum:
                Stream[Int] =
                Stream.empty,*/
                elemOrder: Int = 0
                ):
  //Stream[Int] = {
  Int = {
    assume(elemOrder >= 0, s"'elemOrder' must be positive")
    if (elemOrder == 0) {
      a
    } else if (elemOrder == 1) {
      b
    } else /*if (elemOrder > 1)*/ {
      fibFrom2(
                /*sequence start*/
                a = b,
                b = a + b,
                elemOrder = elemOrder - 1
              )
    }
  }

  /*
  algorithms use multiplication,
  so it become even faster when
  `Karatsuba multiplication` is used.
  it takes Θ(n * logbase2of3) ≈ Θ(n^1.58) time
  instead of quadratic,
  which gives a significant speed-up for large numbers.
   */
  //@tailrec
  def fibonacciDoublingFromRecursive2(
                /*sequence start*/
                a: Int,
                b: Int,
                /*resultAccum:
                Stream[Int] =
                Stream.empty,*/
                elemOrder: Int = 0
                ):
  Int = {
    assume(elemOrder >= 0, s"'elemOrder' must be positive")
    /*
    cases:
    > even
    F(2 * n) = F(n) * (2 * F(n + 1) - F(n))
	  >> odd
	  F(2 * n + 1) = F(n + 1)^2 + F(n)^2
     */
    if (elemOrder == 0) {
      a
    } else if (elemOrder == 1) {
      b
    } else if (elemOrder == 2) {
      a + b
    } else /*if (elemOrder > ?1? 2)*/ {
      if (elemOrder % 2 != 0) {
        //elemOrder > 2 == 2 * (n >=1) + 1
        //3 = 2 * 1 + 1 => F(1 + 1)^2 + F(1)^2
        //5 = 2 * 2 + 1 => F(2 + 1)^2 + F(2)^2
        //>> odd
        val halfOrder: Int = (elemOrder - 1) / 2
        val halfOrderPlusOne: Int = halfOrder + 1
        lazy val fibN: Int =
          fibonacciDoublingFromRecursive2(
                                           a = a,
                                           b = b,
                                           elemOrder = halfOrder
                                         )
        lazy val fibN_PlusOne: Int =
          fibonacciDoublingFromRecursive2(
                                           a = a,
                                           b = b,
                                           elemOrder = halfOrderPlusOne
                                         )

        fibN * fibN + fibN_PlusOne * fibN_PlusOne
      } else /*if (elemOrder % 2 == 0)*/ {
        //elemOrder == 2 * (n >=1)
        /*'1 + 1' `dead` lock*/
        //F(2 * 1) = F(1) * (2 * F(1 + 1) - F(n))
        //> even
        val halfOrder: Int = elemOrder / 2
        val halfOrderPlusOne: Int = halfOrder + 1
        lazy val fibN: Int =
          fibonacciDoublingFromRecursive2(
                                           a = a,
                                           b = b,
                                           elemOrder = halfOrder
                                         )
        lazy val fibN_PlusOne: Int =
          fibonacciDoublingFromRecursive2(
                                           a = a,
                                           b = b,
                                           elemOrder = halfOrderPlusOne
                                         )

        fibN * (2 * fibN_PlusOne - fibN)
      }
    }
  }

  //@tailrec
  def fibonacciDoublingFromRecursive3(
                /*sequence start*/
                a: Int,
                b: Int,
                elemOrder: Int = 0,
                /*must be neutral to operation*/
                fibN: Int = 0,
                fibN_PlusOne: Int = 0,
                                     result: Int = 0
                ):
  Int = {
    assume(elemOrder >= 0, s"'elemOrder' must be positive")
    /*
    cases:
    > even
    F(2 * n) = F(n) * (2 * F(n + 1) - F(n))
	  >> odd
	  F(2 * n + 1) = F(n + 1)^2 + F(n)^2
     */
    /*elemOrder < 2 are base cases
    ? where actual result was return ?
    */
    if (elemOrder == 0) {
      a + result
    } else if (elemOrder == 1) {
      b + result
    } else /*if (elemOrder > 1)*/ {
      if (elemOrder % 2 != 0) {
        //>> odd
        val fibN: Int =
          fibonacciDoublingFromRecursive2(
                                  a = a,
                                  b = b,
                                  elemOrder = elemOrder / 2
                                )
        val fibN_PlusOne: Int =
          fibonacciDoublingFromRecursive2(
                                  a = a,
                                  b = b,
                                  elemOrder = elemOrder / 2 + 1
                                )

        fibN * fibN + fibN_PlusOne * fibN_PlusOne
      } else /*if (elemOrder % 2 == 0)*/ {
        //> even
        val fibN: Int =
        fibonacciDoublingFromRecursive2(
                                /*sequence start*/
                                a = b,
                                b = a + b,
                                elemOrder = elemOrder / 2
                              )
        fibN *
          (
            2 *
              fibonacciDoublingFromRecursive2(
                                      /*sequence start*/
                                      a = b,
                                      b = a + b,
                                      elemOrder = elemOrder / 2 + 1
                                    ) -
              fibN
            )
      }
    }
  }

  /*matrix multiplication operation must be implemented / available*/
  def Matrix_pow(
                  //Matrix
                  A: Stream[Stream[Int]],
                  /*power*/
                  p: Int):
  Stream[Stream[Int]] =
    if (p == 1) {
      //return
      A
    } else if (p % 2 == 1) {
      //return
      A //* Matrix_pow(A, p - 1)
      /*??? else ???*/
    } else {
      //Matrix
      val B: Stream[Stream[Int]] =
        Matrix_pow(A, p / 2)
      //return
      B //* B
    }

  /*
	 * Fast doubling method. Faster than the matrix method.
	 * F(2n) = F(n) * (2*F(n+1) - F(n)).
	 * F(2n+1) = F(n+1)^2 + F(n)^2.
	 * This implementation is the non-recursive version. See the web page and
	 * the other programming language implementations for the recursive version.
	 */
  def fastFibonacciDoubling(
                             /*sequence start*/
                             initA: Int,
                             initB: Int,
                             n: Int
                             ): BigInteger = {
    var a: BigInteger =
      BigInteger
      //.ZERO
      .valueOf(initA.toLong)
    var b: BigInteger =
      BigInteger
      //.ONE
      .valueOf(initB)
    var m: Int = 0

    for (
    //int i = 31 - Integer.numberOfLeadingZeros(n); i >= 0; i--
    /*see
    http://www.tutorialspoint.com/java/lang/integer_numberofleadingzeros.htm
    */
      i <- 31 - Integer.numberOfLeadingZeros(n) to 0 by -1
    ) {
      // Loop invariant: a = F(m), b = F(m+1)
      //assert( a.equals(slowFibonacci(m)))
      //assert( b.equals(slowFibonacci(m+1)))

      // Double it
      val d: BigInteger =
        a.multiply(
                    b
                    .shiftLeft(1)
                    .subtract(a)
                  )
      val e: BigInteger =
        a.multiply(a)
        .add(b.multiply(b))
      a = d
      b = e
      m *= 2
      //assert( a.equals(slowFibonacci(m)))
      //assert( b.equals(slowFibonacci(m+1)))

      // Advance by one conditionally
      if (((n >>> i) & 1) != 0) {
        val c: BigInteger = a.add(b)
        a = b
        b = c
        m += 1
        //assert( a.equals(slowFibonacci(m)))
        //assert( b.equals(slowFibonacci(m+1)))
      }
    }
    //return
    a
  }
}
