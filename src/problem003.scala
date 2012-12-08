/**
 * User: Vincent DOBA
 * Date: 28/05/11
 * Time: 16:50
 */

/**
 * === Problem n°3 ===
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 *
 */

def problem3(x: BigInt, y:BigInt):BigInt ={
  if (x==y)
    y
  else if ((y % x)==0)
    problem3(x,y/x)
  else
    problem3(x+1,y);
}

println(problem3(2, 600851475143L))

// answer is 6857