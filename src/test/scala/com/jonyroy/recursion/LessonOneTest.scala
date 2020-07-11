package com.jonyroy.recursion

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import LessonOne._

class LessonOneTest extends AnyFunSuite with Matchers {

  test("UnitTest for generateNthNaturalNumbersAsc") {
    generateNthNaturalNumbersAsc(10) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }

  test("UnitTest for generateNthNaturalNumbersDsc") {
    generateNthNaturalNumbersDsc(10) shouldBe List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  }

  test("UnitTest for findSumOfNthNumbers") {
    findSumOfNthNumbers(10) shouldBe 55
  }

  test("UnitTest for findNthFibonacciNumber") {
    findNthFibonacciNumber(6) shouldBe (8)
    the[IllegalArgumentException] thrownBy findNthFibonacciNumber(21) should have message "requirement failed: Invalid Input"
  }

  test("UnitTest for countDigits") {
    countDigits(123456789) shouldBe 9
  }

  test("UnitTest for findSumOfDigits") {
    findSumOfDigits(123456789) shouldBe 45
  }

  test("UnitTest for findGCD") {
    findGCD(15, 20) shouldBe 5
    findGCD(112, 100) shouldBe 4
    findGCD(37, 101) shouldBe 1
  }

  test("UnitTest for findLargestElem") {
    findLargestElem(List[Long](1, 2, 4, 6, -89, 238475, -83736)) shouldBe 238475
  }

  test("UnitTest for findSmallestElem") {
    findSmallestElem(List[Long](1, 2, 4, 6, -89, 238475, -83736)) shouldBe -83736
  }

  test("UnitTest for reverseString") {
    reverseString("reverseString") shouldBe "gnirtSesrever"
  }

}
