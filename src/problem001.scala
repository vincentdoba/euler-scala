/**
 * User: Vincent DOBA
 * Date: 28/05/11
 * Time: 16:50
 */

/**
 * === Problem n°1 ===
 *
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
 * The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 */

def problem1(x :BigInt): BigInt = {
  if (x==0)
    0
  else if ((x % 3)==0 || (x%5)==0)
    x + problem1(x-1)
  else
    problem1(x-1);
}

println(problem1(999))

// answer is 233168