/**
 * User: Vincent DOBA
 * Date: 29/05/11
 * Time: 23:26
 */

/**
 * === Problem n°10 ===
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 *
 */

def Prime(list1:List[Int], sum:Long):Long = list1 match {
    case Nil => sum;
    case x::xs if (x==1) => Prime(xs, sum);
    case x::xs if (x*x > list1.last) => sumOfList(x::xs, sum);
    case x::xs => Prime(xs.filter(element => element % x != 0), x + sum);
}

def sumOfList(list1:List[Int], sum:Long):Long = list1 match {
  case Nil => sum
  case x::xs => sumOfList(xs, x+sum)
}

def problem010(n:Int):Long = Prime(List.range(2,n), 0)

println(problem010(2000000))

// Answer is 142913828922