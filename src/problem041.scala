/**
 * User: Vincent DOBA
 * Date: 28/06/11
 * Time: 00:10
 */

/**
 * === Problem n°40 ===
 *
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
 * For example, 2143 is a 4-digit pandigital and is also prime.
 *
 * What is the largest n-digit pandigital prime that exists?
 *
 */

/**
 * Given an incremental list of int, beginning at 1, return all the prime numbers in the list
 */
def Prime(list1:List[Int], list2:List[Int]):List[Int] = list1 match {
    case Nil => list2;
    case x::xs if (x==1) => Prime(xs, list2);
    case x::xs if (x*x > list1.last) => list2.reverse:::list1;
    case x::xs => Prime(xs.filter(element => element % x != 0), x::list2);
}

/**
 * Sieve of Eratosthenes
 */
def eratosthenes(n:Int):List[Int] = Prime(List.range(2,n), Nil)


def intToList(n:Int, result:List[Int]):List[Int] = n match {
  case 0 => result
  case n => intToList(n/10, (n%10)::result)
}

def isPandigital(n:List[Int], digits:Array[Int]):Boolean = n match {
  case Nil => true
  case x::xs if (x > (digits.size + n.size)) => false
  case x::xs if (digits.contains(x)) => false;
  case x::xs => isPandigital(xs, digits :+ x)
}

def maxPandigital(candidate:List[Int]):Int = candidate match {
  case Nil => 0
  case x::xs if (isPandigital(intToList(x, Nil), Array())) => x
  case x::xs => maxPandigital(xs)
}

print(maxPandigital(eratosthenes(7654321).reverse))

// Answer is 7652413