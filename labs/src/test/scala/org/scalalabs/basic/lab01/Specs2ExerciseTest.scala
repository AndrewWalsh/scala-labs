package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import java.lang.{ IllegalArgumentException }

/**
 * In this Lab you will implement a Specs2 testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a Specs2 specification to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class Specs2ExerciseTest extends Specification {
  "specs2" should {
    "throw IllegalArgumentException when divisor is <= 0" in {
      val e = new Euro(1, 2)
      def throws = { e / 0 }
      throws must throwA[IllegalArgumentException]
    }

    "return correct result when divided" in {
      val e = new Euro(3, 0) / 2
      e.euro ==== 1
      e.cents ==== 50
    }
  }
}
