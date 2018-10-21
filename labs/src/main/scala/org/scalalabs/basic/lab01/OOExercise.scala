package org.scalalabs.basic.lab01
import scala.language.implicitConversions
import org.scalalabs.basic.lab01.DefaultCurrencyConverter

/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 *
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 *
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method
 *
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Euro to Dollar using the
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */
abstract trait Currency {
  val symbol = "EUR"
}

class Euro(val euro: Int, val cents: Int = 0) extends Currency with Ordered[Euro] {
  require(cents <= 100 && cents >= 0)
  def compare(that: Euro) =  this.euro - that.euro

  val inCents = euro * 100 + cents

  private def euroDiff = (diffCents: Int) => diffCents / 100

  override def toString = symbol + ": " + euro + ',' + (if (cents > 0) f"$cents%02d" else "--")

  def + = (OtherEuro: Euro) => {
    val diff = euroDiff(cents + OtherEuro.cents)
    val addCents = (cents + OtherEuro.cents) - (diff * 100)
    val addEuro = diff + OtherEuro.euro + euro
    new Euro(addEuro, addCents)
  }

  def * = (timesBy: Int) => {
    val totalEuro = euro * timesBy
    val totalCents = cents * timesBy
    val diff = euroDiff(totalCents)
    new Euro(totalEuro + diff, totalCents - diff * 100)
  }
}

object Euro {
  implicit def apply(euro: Int, cents: Int) { new Euro(euro, cents) }
  def fromCents: Int => Euro = cents => {
    val euro = cents / 100
    val newCents = cents - euro * 100
    new Euro(euro, newCents)
  }

  implicit class timesInt(timesBy: Int) {
    def * (e: Euro): Euro = e * timesBy
  }

  implicit def dollarToEuro(d: Dollar): Euro = {
    Euro.fromCents(DefaultCurrencyConverter.toEuroCents(d.toCents))
  }
}

class Dollar(dollars: Int, cents: Int) {
  def toCents: Int = dollars * 100 + cents
}
