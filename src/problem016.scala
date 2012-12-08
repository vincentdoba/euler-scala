/**
 * User: Vincent DOBA
 * Date: 30/05/11
 * Time: 00:10
 */

/**
 * === Problem n°16 ===
 *
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^1000?
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
 * return p^n in a list structure
 */
def power(result:List[Int], p:Int, n:Int):List[Int] = n match {
  case 0 => result
  case n => power(multiply(result.reverse, Nil, 0, p), p, n-1)
}

def sumOfList(list1:List[Int], sum:Long):Long = list1 match {
  case Nil => sum
  case x::xs => sumOfList(xs, x+sum)
}

def problem16(p:Int, n:Int) = sumOfList(power(List(1), p, n), 0)

println(problem16(2, 1000))

// Answer is 1366