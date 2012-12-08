/**
 * User: Vincent DOBA
 * Date: 28/05/11
 * Time: 20:50
 */

/**
 * === Problem n°5 ===
 *
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 *
 */


/**
 * Given an incremental list of int, beginning at 1, return all the prime numbers in the list
 */
def Prime(list1:List[Int], list2:List[Int]):List[Int] = list1 match {
    case Nil => list2;
    case x::xs if (x==1) => Prime(xs, list2);
    case x::xs if (x*x > list1.last) => list2:::list1;
    case x::xs => Prime(DropMultiples(x,xs), x::list2);
}

/**
 * Given an int n and a list list1, return the list without all the multiples of n
 */
def DropMultiples(n:Int, list1:List[Int]):List[Int] = {
  if ((list1.isEmpty) || n==1)
    list1
  else if (list1.head % n ==0)
    DropMultiples(n, list1.drop(1))
  else
    list1(0)::DropMultiples(n, list1.drop(1))
}

/**
 * Given an int n, return a list from 1 to n
 */
def createIncrementalList(n:Int):List[Int] = n match {
  case 0 => Nil;
  case n => createIncrementalList(n-1):::List(n);
}

/**
 * Sieve of Eratosthenes
 */
def eratosthenes(n:Int):List[Int] = Prime(createIncrementalList(n), Nil)

def greatestPowerUnder(limit:Int, n:Int, previousPower:Int):Int = previousPower match {
  case 0 => 0;
  case previousPower if (previousPower*n > limit) => previousPower;
  case previousPower => greatestPowerUnder(limit, n, previousPower*n);
}

def multiply(list:List[Int]):Int = list match {
  case Nil => 1;
  case x::xs => x*multiply(xs);
}

def problem5(n:Int):Int = {
  multiply(eratosthenes(n).map({ x => greatestPowerUnder(n,x,1)}))
}

println(problem5(20));

// Answer is 232792560