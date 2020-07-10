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
        case 0      => acc
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
        case 1       => 1
        case 2       => 1
        case n: Long => _loop(n - 1) + _loop(n - 2)
      }
    }

    _loop(nthFibNum)
  }

  /**
    * Write a program to count the digits of a given number
    *
    * @param num
    * @return Number of digits of a given number
    */
  def countDigits(num: Long): Long = {
    @scala.annotation.tailrec
    def _loop(num: Long, acc: Int): Int = {
      num match {
        case 0       => acc
        case n: Long => _loop(n / 10, acc + 1)
      }
    }

    _loop(num, 0)
  }

  /**
    * Write a program to find the sum of digits of a number
    *
    * @param num
    * @return sum of digits of a number
    */
  def findSumOfDigits(num: Long): Int = {
    @scala.annotation.tailrec
    def _loop(n: Long, acc: Int): Int = {
      n match {
        case 0       => acc
        case n: Long => _loop(n / 10, (acc + n % 10).toInt)
      }
    }

    _loop(num, 0)
  }

  /**
    *
    * Write a program to find GCD of two numbers
    *
    * @param dividend
    * @param divisor
    * @return greatest common divisor of two numbers
    */
  @scala.annotation.tailrec
  def findGCD(dividend: Long, divisor: Long): Long = {
    divisor match {
      case 0             => dividend
      case divisor: Long => findGCD(divisor, dividend % divisor)
    }
  }

  /**
    * Write a program to get the largest element of an List
    *
    * @param listOfNum
    * @return the largest element of an List
    */
  def findLargestElem(listOfNum: List[Long]): Long = {
    def _loop(listOfNum: List[Long], maxNum: Long): Long = {
      listOfNum match {
        case elem :: tail => _loop(tail, if (elem > maxNum) elem else maxNum)
        case _            => maxNum
      }
    }

    _loop(listOfNum, Long.MinValue)
  }

  /**
    * Write a program to get the smallest element of an List
    *
    * @param listOfNum
    * @return the smallest element of an List
    */
  def findSmallestElem(listOfNum: List[Long]): Long = {
    def _loop(listOfNum: List[Long], minNum: Long): Long = {
      listOfNum match {
        case elem :: tail => _loop(tail, if (elem < minNum) elem else minNum)
        case _            => minNum
      }
    }

    _loop(listOfNum, Long.MaxValue)
  }

}
