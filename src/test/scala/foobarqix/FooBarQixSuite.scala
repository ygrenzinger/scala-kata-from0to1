package foobarqix

import foobarqix.FooBarQix.convert
import org.scalatest.FunSuite

class FooBarQixSuite extends FunSuite {

  test("Should return number converted to string") {
    assert(convert(1) == "1")
  }

  test("Should return Foo when number is only divisible by 3") {
    assert(convert(6) == "Foo")
  }

  test("Should return Foo* because number is only divisible by 5 and contains 0") {
    assert(convert(10) == "Bar*")
  }

  test("Should return Foo when number is only divisible by 7") {
    assert(convert(14) == "Qix")
  }

  test("Should return FooBarQix* because number is divisible by 3 5 7 and contains 0") {
    assert(convert(210) == "FooBarQix*")
  }

  test("Should return FooBarQix when number contains digit 3 5 7") {
    assert(convert(63571) == "FooBarQix")
  }

  test("Should return FooBarQix*Bar") {
    assert(convert(105) == "FooBarQix*Bar")
  }

  test("Should return 1*1") {
    assert(convert(101) == "1*1")
  }

}