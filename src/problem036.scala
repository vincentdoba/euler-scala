/**
 * User: Vincent DOBA
 * Date: 27/06/11
 * Time: 00:10
 */

/**
 * === Problem n°36 ===
 *
 * The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
 *
 * Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
 *
 */

def sumOfPalindromicNumber(n:Int, sum:Int):Int = n match {
  case 0 => sum
  case n if (n.toString.reverse.equals(n.toString) &&
    Integer.toBinaryString(n).reverse.equals(Integer.toBinaryString(n))) => sumOfPalindromicNumber(n-1, sum + n)
  case n => sumOfPalindromicNumber(n-1, sum)
}

def problem36(n:Int) = sumOfPalindromicNumber(n, 0)

print(problem36(1000000))


// Answer is 872187