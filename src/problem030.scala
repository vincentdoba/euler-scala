/**
 * User: Vincent DOBA
 * Date: 19/06/11
 * Time: 00:10
 */

/**
 * === Problem n°30 ===
 *
 * Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
 *
 *   1634 = 1^4 + 6^4 + 3^4 + 4^4
 *   8208 = 8^4 + 2^4 + 0^4 + 8^4
 *   9474 = 9^4 + 4^4 + 7^4 + 4^4
 *
 * As 1 = 1^4 is not a sum it is not included.
 *
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 *
 * Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
 *
 */

def sumOfDigit(n:Int, p:Int,  sum:Int):Int = n match {
  case n if (n<10) => sum + math.pow(n, p).toInt
  case n => sumOfDigit(n/10, p, sum + math.pow((n % 10), p).toInt)
}

def sumOfNumber(n:Int, p:Int, sum:Int):Int = n match {
  case 1 => sum
  case n if (n == sumOfDigit(n, p, 0)) => sumOfNumber(n-1, p, sum + n)
  case n => sumOfNumber(n-1, p, sum)
}

def determineUpperBound(p:Int, upperBound:Int):Int = upperBound match {
  case n if (numberOfDigits(n*math.pow(9,p).toInt,0)<n) => math.pow(9,p).toInt*n
  case n => determineUpperBound(p, upperBound +1)
}

def numberOfDigits(n:Int, result:Int):Int = n match {
  case n if (n<10) => result+1
  case n => numberOfDigits(n/10, result+1)
}

def problem30(p:Int):Int = sumOfNumber(determineUpperBound(p, 1), p, 0)

print(problem30(5))

// Answer is 443839