/**
 * User: Vincent DOBA
 * Date: 27/06/11
 * Time: 00:10
 */

/**
 * === Problem n°28 ===
 *
 * Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
 *
 * 21 22 23 24 25
 * 20  7  8  9 10
 * 19  6  1  2 11
 * 18  5  4  3 12
 * 17 16 15 14 13
 *
 * It can be verified that the sum of the numbers on the diagonals is 101.
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
 *
 */


def computeSpiralSum(spiralSize:Int, sum:Int):Int = spiralSize match {
  case 0 => sum + 1
  case n => computeSpiralSum(n-1, sum + 16*n*n + 4*n + 4)
}

def problem28(n:Int) = computeSpiralSum((n-1)/2, 0)

print(problem28(1001))


// Answer is 669171001