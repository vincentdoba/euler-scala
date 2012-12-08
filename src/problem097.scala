/**
 * User: Vincent DOBA
 * Date: 17/06/11
 * Time: 00:10
 */

/**
 * === Problem n°97 ===
 *
 * The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime
 * of the form 2^6972593−1; it contains exactly 2,098,960 digits.
 * Subsequently other Mersenne primes, of the form 2^p−1,
 * have been found which contain more digits.
 *
 * However, in 2004 there was found a massive non-Mersenne prime which
 * contains 2,357,207 digits: 28433×2^7830457+1.
 *
 * Find the last ten digits of this prime number.
 *
 */

/**
 * return the last ten digits of number^p. result is here for recursion and should
 * be initialized at 1.
 */
def power(number:Int, p:Int, result:BigInt):BigInt = p match {
  case 0 => result
  case p => power(number, p-1, (result*number) % BigInt(10000000000L))
}

def problem97() = (BigInt(28433)*power(2, 7830457, 1) + 1) % BigInt(10000000000L)

print(problem97())

// Answer is 8739992577