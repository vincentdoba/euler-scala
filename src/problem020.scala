/**
 * User: Vincent DOBA
 * Date: 30/05/11
 * Time: 00:10
 */

/**
 * === Problem n°20 ===
 *
 * n! means n × (n − 1) × ... × 3 × 2 × 1
 *
 * For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
 * and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 *
 * Find the sum of the digits in the number 100!
 *
 */

/**
 * Multiply a number (number1) represented as an inverted list with another number (number2)
 * rest and result are for recursion and then should be initialized at 0 and Nil
 */
def multiply(number1:List[Int], result:List[Int], rest:Int, number2:Int):List[Int] = (number1, rest) match {
  case (Nil, 0) => result
  case (Nil, rest) if (rest<10) => rest::result
  case (Nil, rest) => multiply(Nil, (rest % 10)::result, rest/10, number2)
  case (x::xs, rest) => multiply(xs, computeUnit(x,number2,rest)::result, computeRest(x,number2,rest), number2)
}

/**
 * return the unit number of the result of a multiplication between 2 numbers under 10
 * rest should be initialized at 0
 */
def computeUnit(x:Int, p:Int, rest:Int):Int = (x*p + rest) % 10

/**
 * return the decimal number of the result of a multiplication between 2 numbers under 10
 * rest should be initialized at 0
 */
def computeRest(x:Int, p:Int, rest:Int):Int = ((x*p+rest) - (x*p + rest) % 10)/10

/**
 * return n! in a list structure
 */
def factorial(result:List[Int], n:Int):List[Int] = n match {
  case 0 => result
  case n => factorial(multiply(result.reverse, Nil, 0, n), n-1)
}

println(factorial(List(1), 30))

def sumOfList(list1:List[Int], sum:Long):Long = list1 match {
  case Nil => sum
  case x::xs => sumOfList(xs, x+sum)
}

def problem20(n:Int) = sumOfList(factorial(List(1), n), 0)

println(problem20(100))

// Answer is 648