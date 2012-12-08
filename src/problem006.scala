/**
 * User: Vincent DOBA
 * Date: 28/05/11
 * Time: 23:34
 */

/**
 * === Problem n°6 ===
 *
 * The sum of the squares of the first ten natural numbers is,
 * 1^2 + 2^2 + ... + 10^2 = 385
 *
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)^2 = 552 = 3025
 *
 * Hence the difference between the sum of the squares of the first ten natural numbers
 * and the square of the sum is 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers
 * and the square of the sum.
 *
 */

def sumOfFirstn(n:Int):Int = (n*(n+1))/2

def problem6(n:Int, x:Int):Int = x match {
  case 0 => 0
  case x => (sumOfFirstn(n)-x)*x + problem6(n, x-1)
}

println(problem6(100, 100));

// Answer is 25164150
