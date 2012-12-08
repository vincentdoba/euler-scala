/**
 * User: Vincent DOBA
 * Date: 17/06/11
 * Time: 00:10
 */

/**
 * === Problem n°48 ===
 *
 * The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
 *
 * Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
 *
 *
 */


/**
 * return the last ten digits of number^p
 * result is here for recursion and should be initialized at 1
 */
def power(number:Int, p:Int, result:BigInt):BigInt = p match {
  case 0 => result
  case p => power(number, p-1, (result*number) % BigInt(10000000000L))
}

/**
 * return the last ten digits of the series 1^1 + 2^2 + 3^3 + ... + number^number
 * result is here for recursion and should be initialized at 0
 */
def sum(number:Int, result:BigInt):BigInt = number match {
  case 0 => result
  case n => sum(n-1, (result + power(n, n, 1)) % BigInt(10000000000L))
}

def problem48(n:Int) = sum(n, 0)

print(problem48(1000))

// Answer is 9110846700