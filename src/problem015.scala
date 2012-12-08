/**
 * User: Vincent DOBA
 * Date: 19/06/11
 * Time: 00:10
 */

/**
 * === Problem n°15 ===
 *
 * Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking)
 * to the bottom right corner.
 *
 * How many routes are there through a 20×20 grid ?
 *
 */

def factorial(n:Int, result:Long):Long = n match {
  case 0 => result
  case n => factorial(n-1, result*n)
}

def binomialCoeff(n:Int, k:Int):Long = factorial(n, 1)/(factorial(n-k, 1)*factorial(k, 1))

def numberOfPaths(n:Int, iteration:Int, sum:Long):Long = iteration match {
  case -1 => sum
  case iteration => numberOfPaths(n, iteration-1, sum + binomialCoeff(n, iteration)*binomialCoeff(n, iteration))
}

def problem15(n:Int):Long = numberOfPaths(n, n, 0)

print(problem15(20))

// Answer is 137846528820