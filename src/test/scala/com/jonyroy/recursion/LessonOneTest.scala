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
}
