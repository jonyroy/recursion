package com.jonyroy.recursion

object LessonOne {

  /**
    * Write a program to generate Nth Natural Numbers by Ascending Order
    *
    * @param nthNum nth Natural Number
    * @return list of Int by Ascending Order
    */
  def generateNthNaturalNumbersAsc(nthNum: Int): List[Int] = {
    @scala.annotation.tailrec
    def _loop(nthNum: Int, acc: List[Int]): List[Int] = {
      nthNum match {
        case 0      => acc
        case n: Int => _loop(nthNum - 1, n :: acc)
      }
    }
    _loop(nthNum, List[Int]())
  }

  /**
    * Write a program to generate Nth Natural Numbers by Descending Order
    *
    * @param nthNum nth Natural Number
    * @return list of Int by Descending Order
    */
  def generateNthNaturalNumbersDsc(nthNum: Int): List[Int] = {
    @scala.annotation.tailrec
    def _loop(nth: Int, acc: List[Int]): List[Int] = {
      nth match {
        case n: Int if n == nthNum + 1 => acc
        case n: Int                    => _loop(nth + 1, n :: acc)
      }
    }
    _loop(1, List[Int]())
  }

  /**
   * Write a program to calculate the sum of numbers from 1 to n using recursion
   *
   * @param nthNum nth Natural Number
   * @return sum of numbers from 1 to n
   */
  def findSumOfNthNumbers(nthNum: Int): Int = {
    @scala.annotation.tailrec
    def _loop(nth: Int, acc: Int): Int = {
      nth match {
        case 0 => acc
        case n: Int => _loop(n - 1, acc + n)
      }
    }

    _loop(nthNum, 0)
  }

  /**
   * Write a program to find Nth Fibonacci Number
   * Complexity:
   * @param nthFibNum nth Natural Number
   * @return nth Fibonacci Number
   */
  def findNthFibonacciNumber(nthFibNum: Long): Long = {

    require(nthFibNum < 20, "Invalid Input")

    def _loop(nth: Long): Long = {
      nth match {
        case 1 => 1
        case 2 => 1
        case n: Long => _loop(n - 1) + _loop(n - 2)
      }
    }

    _loop(nthFibNum)
  }

}
