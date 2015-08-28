val n0: Int = 0
val n1: Int = 1
val n2: Int = 2
val n3: Int = 3
val n4: Int = 4
val n5: Int = 5
2 * n0
2 * n0 + 1
2 * n1
2 * n1 + 1
2 * n2
2 * n2 + 1
2 * n3
2 * n3 + 1
2 * n4
2 * n4 + 1
2 * n5
2 * n5 + 1
/*
The
java.lang.Integer.numberOfLeadingZeros() method
returns
the number of
zero bits preceding the highest-order
("leftmost") one-bit in
the two's complement binary representation of
the specified int value.
It returns
'32' if
the specified value has no one-bits
in its two's complement representation,
in other words
if
it is equal to zero.
 */
val str32Zeros: String = "0" * 32
31 - Integer.numberOfLeadingZeros(5)
Integer.numberOfLeadingZeros(0)
val i: Int = 170
"Number = " + i
/* returns
the string representation of
the unsigned integer value
represented by
the argument in binary (base 2) */
"Binary = " + Integer.toBinaryString(i)
// returns the number of one-bits
"Number of one bits = " + Integer.bitCount(i)
/* returns
an int value with
at most a single one-bit,
in the position
of the highest-order ("leftmost") one-bit
in the specified int value */
"Highest one bit = " + Integer.highestOneBit(i)
/* returns
an int value with
at most a single one-bit,
in the position
of the lowest-order ("rightmost") one-bit in
the specified int value.*/
"Lowest one bit = " + Integer.lowestOneBit(i)
/*returns
the number of zero bits
preceding
the highest-order ("leftmost") one-bit */
"Number of leading zeros = " +
  Integer.numberOfLeadingZeros(i)