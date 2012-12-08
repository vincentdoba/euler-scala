/**
 * User: Vincent DOBA
 * Date: 18/06/11
 * Time: 00:10
 */

/**
 * === Problem n°14 ===
 *
 * The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following sequence:
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
 * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 */

/**
 * return the next term of the sequence, given n the current term
 */
def next(n:Long):Long = n match {
  case n if ((n%2)==0) => n/2
  case n => 3L*n + 1L
}

/**
 * Return an array of size n+1 where the element at index x contains the length of the sequence starting by x
 * n, iterator should be initialized at the same value
 * list is here for recursion and should by initialized at Nil
 * array should be an empty array of size n+1
 */
def computeLength(n:Long , iterator:Int, list:List[Int], array:Array[Int]):Array[Int] = (iterator, n) match {
  case (0, n) => array
  case (iterator, 1) => computeLength(iterator-1, iterator-1, Nil, updateArray(list, 1, array))
  case (iterator, n) if (n>=array.length) => computeLength(next(n), iterator, n.intValue::list , array)
  case (iterator, n) if (array.apply(n.intValue)>0) => computeLength(iterator-1, iterator-1,
    Nil, updateArray(list, array.apply(n.intValue)+1, array))
  case (iterator, n) => computeLength(next(n), iterator, n.intValue::list, array)
}

/**
 * update all the elements of array whose index is in list list with the length of the sequence
 */
def updateArray(list:List[Int], value:Int, array:Array[Int]):Array[Int] = list match {
  case Nil => array
  case x::xs if (x >= array.length || x<0) => updateArray(xs, value + 1, array)
  case x::xs => array.update(x, value)  ;updateArray(xs, value + 1, array)
}

/**
 * Given an array, return the index of the element whose value is the maximum
 * Iterator and index should be initialized at 0
 */
def selectBestValue(iterator:Int, index:Int, array:Array[Int]):Int = iterator match {
  case iterator if (iterator>=array.length) => index
  case iterator if (array.apply(iterator)>array.apply(index)) => selectBestValue(iterator+1, iterator, array)
  case iterator => selectBestValue(iterator+1, index, array)
}

def problem14(n:Long):Int = selectBestValue(0, 0, computeLength(n, n.intValue, Nil, new Array(n.intValue+1)))

print(problem14(1000000L))


// Answer is 837799