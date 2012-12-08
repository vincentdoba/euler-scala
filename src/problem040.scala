/**
 * User: Vincent DOBA
 * Date: 28/06/11
 * Time: 00:10
 */

/**
 * === Problem n°40 ===
 *
 * An irrational decimal fraction is created by concatenating the positive integers:
 *
 * 0.123456789101112131415161718192021...
 *
 * It can be seen that the 12th digit of the fractional part is 1.
 *
 * If d_n represents the nth digit of the fractional part, find the value of the following expression.
 *
 * d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d1_000000
 *
 */

def seqConstruct(n:Int, value:String, limit:Int):String = value match {
  case value if (value.length>limit) => value
  case value => seqConstruct(n+1,value + n.toString, limit)
}

def product(n:Int, value:String, result:Int):Int = n match {
  case n if (n < 10) => result*(value(n).toInt - 48)
  case n => product(n/10, value, result*(value(n).toInt - 48))
}

def problem40(n:Int):Int = product(n, seqConstruct(0, "", n), 1)

print(problem40(1000000))


// Answer is 210