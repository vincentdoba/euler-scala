/**
 * User: Vincent DOBA
 * Date: 29/05/11
 * Time: 14:58
 */

/**
 * === Problem n°9 ===
 *
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * a^2 + b^2 = c^2
 *
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 *
 */

def findPythagore(a:Int, b:Int, c:Int):(Int, Int, Int) = (a,b,c) match {
  case (a,b,c) if (a*a + b*b == c*c) => (a,b,c)
  case (a,b,c) if (c<0) => (a,b,c)
  case (a,b,c) if (b>c) => findPythagore(a+1, a+2, 1000 - (a+1) - (a+2))
  case (a,b,c) => findPythagore (a, b+1, c-1)
}

def multiply(a:(Int, Int, Int)):Int = a._1*a._2*a._3

def problem009(n:Int):Int = multiply(findPythagore(1,2,n-3))

println(problem009(1000))

// Answer is 31875000