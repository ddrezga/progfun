package recfun
import common._

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    
    val tr = measureExec(for (i <- 0 until 100000000) factorial(25,1))
    val ntr = measureExec(for (i <- 0 until 100000000) factorialnt(25))
    
    println("Tail recursive : " + tr)
    println("Recursive : " + ntr)
  }
  
  def measureExec(u: => Any) : Long = {
    val ts = System.currentTimeMillis()
    u
    System.currentTimeMillis() - ts
  }
  
  def factorial(n: Long, acc: Long): Long = {
    if (n == 0) acc else factorial(n-1, acc * n)
  }

  def factorialnt(n: Long): Long = {
    if (n == 0) 1 else n * factorialnt(n-1)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): BigInt = {
//    if (c == 0 || r == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1) // recursive
    if (c == 0 || r == 0 || c == r) 1 else factorial(r,1) / (factorial(c, 1) * factorial(r - c, 1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iter(chars: List[Char], b: Int): Boolean = {
      if (chars.isEmpty) b == 0
      else
        if (b < 0) false else iter(chars.tail, b + (if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0))
    }
    iter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =  {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
