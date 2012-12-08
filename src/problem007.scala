/**
 * User: Vincent DOBA
 * Date: 29/05/11
 * Time: 12:54
 */

/**
 * === Problem n°7 ===
 *
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10001st prime number?
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

def problem7(n:Int):Int = {
  val list = eratosthenes(n*20)
  list.apply(n-1)
}

println(problem7(10001))

// Answer is 104743