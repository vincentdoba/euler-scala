/**
 * User: Vincent DOBA
 * Date: 18/06/11
 * Time: 00:10
 */

/**
 * === Problem n°92 ===
 *
 * A number chain is created by continuously adding the square of the digits in a number to form a new number
 * until it has been seen before.
 *
 * For example,
 *
 * 44 → 32 → 13 → 10 → 1 → 1
 * 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
 *
 * Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
 * What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
 *
 * How many starting numbers below ten million will arrive at 89?
 *
 */

/**
 * Compute the sum of the digits power two of a number n. the variable sum is here for recursion and
 * should be initialized at 0
 */
def powerSum(n:Int, sum:Int):Int = n match {
  case n if (n<10) => sum + n*n
  case n => powerSum(n/10, sum + (n % 10)*(n % 10))
}

/**
 * compute how many starting numbers below n will arrive at 89
 * n and iterator should have the same value, list and sum are here for recursion and should be
 * initialized at Nil and 0. array should be an empty array of size numberOfDigitsInN * 81 + 1
 */
def computeSum(n:Int, iterator:Int, list:List[Int], array:Array[Int], sum:Int):Int = (iterator, n) match {
  case (0, n) => sum
  case (iterator, 1) => computeSum(iterator-1, iterator-1, Nil, updateArray(list, 1, array), sum)
  case (iterator, 89) => computeSum(iterator-1, iterator-1, Nil, updateArray(list, 89, array), sum+1)
  case (iterator, n) if (n>=array.length) => computeSum(powerSum(n,0), iterator, list , array, sum)
  case (iterator, n) if (array.apply(n)==1) => computeSum(iterator-1, iterator-1,
    Nil, updateArray(list, 1, array), sum)
  case (iterator, n) if (array.apply(n)==89) => computeSum(iterator-1, iterator-1,
    Nil, updateArray(list, 89, array), sum+1)
  case (iterator, n) => computeSum(powerSum(n,0), iterator, n::list, array, sum)
}

/**
 * Put the value value into each array element whose index is contained into the list
 */
def updateArray(list:List[Int], value:Int, array:Array[Int]):Array[Int] = list match {
  case Nil => array
  case x::xs if (x >= array.length) => updateArray(xs, value, array)
  case x::xs => array.update(x, value)  ;updateArray(xs, value, array)
}

/**
 * given a number n, construct an array of size numberOfDigitsInN * 81 + 1
 */
def constructArray(n:Int, sum:Int):Array[Int] = n match {
  case n if (n < 10) => new Array((sum+1)*81 + 1)
  case n => constructArray(n/10, sum+1)
}


def problem92(n:Int):Int = computeSum(n, n, Nil, constructArray(n, 0), 0)

print(problem92(10000000))


// Answer is 8581146