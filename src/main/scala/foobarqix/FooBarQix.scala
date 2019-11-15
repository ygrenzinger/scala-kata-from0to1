package foobarqix

object FooBarQix {

  val conversions: Map[Int, String] = Map(3 -> "Foo", 5 -> "Bar", 7 -> "Qix")

  def convert(number: Int): String = {
    val divisiblePart = convertForDivisiblePart(number)
    val digits = number.toString.map(c => c.asDigit)

    if (!divisiblePart.isEmpty || digits.exists(conversions.contains)) {
      divisiblePart ++ convertForDigitPart(digits)
    } else {
      number.toString.map(c => if (c == '0') { '*' } else { c })
    }
  }

  def main(args: Array[String]): Unit = {
    LazyList.from(1).take(100).foreach(x => println(convert(x)))
  }

  private def isDivisibleBy(number: Int, divisor: Int): Boolean = number % divisor == 0

  private def convertForDigitPart(digits: Iterable[Int]): String =
    digits.flatMap(d => if (d == 0) { Some('*') } else { conversions.get(d) }).mkString("")

  private def convertForDivisiblePart(number: Int): String = {
    conversions.keys
      .filter(isDivisibleBy(number, _))
      .flatMap(conversions.get)
      .mkString("")
  }

}
