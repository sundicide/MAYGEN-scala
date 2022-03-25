import MAYGEN.Utils
import org.junit.Assert._
import org.junit._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

class UtilsTestSuite {
  trait TestSets {
    val testSmiles = "c2O6H2"
    val normalizedSmiles = Utils.normalizeFormula(testSmiles)
    val normalizedSmilesAtoms = Array("C2", "O6", "H2")

    val abnormalSmiles = "asdqwe"
    val normalizedAbnormalSmiles = Utils.normalizeFormula(abnormalSmiles)
  }

  @Test def validateSmiles(): Unit = {
    new TestSets {
      //      assertTrue(Utils.validateFormula(normalizedSmiles))
      //      assertFalse(Utils.validateFormula(normalizedAbnormalSmiles))
    }
  }

  @Test def splitByAtoms(): Unit = {
    new TestSets {
      val result = Utils.splitByAtoms(normalizedSmiles)
      assertTrue(result.sameElements(normalizedSmilesAtoms))
    }
  }

  @Test def sum(): Unit = {
    val testList = Array(1,2,3,4)
    println(Utils.sum(testList, 2))
  }

  @Test def getHydrogensInfoLengthIsOne(): Unit = {
    val formula = "C2H6O2H10"
    val expectation = 16
    val result = Utils.getHydrogensCount(formula)
    assertEquals(result, expectation)
  }

  @Test def getOnlySymbols(): Unit = {
    val formula = "C2H10O5"
    val result = Utils.getOnlySymbols(formula)
    val expectation = Array("C","C", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "O", "O", "O", "O", "O")
    assertTrue(expectation.sameElements(result))
  }

  @Test def makeAtomString(): Unit = {
    assertTrue(List("C", "C", "C").equals(Utils.makeOneAtomSerialize("C3")))
    assertTrue(List("O", "O").equals(Utils.makeOneAtomSerialize("O2")))
  }

  @Test def makeAtomMap(): Unit = {
    val testSmiles = "C2O6C2"
    val ss = Utils.makeAtomHashMap(testSmiles)
    val expectationMap: HashMap[String, Int] = HashMap("C" -> 4, "O" -> 6)
    assertTrue(expectationMap.equals(ss))
  }

  @Test def getOccurences(): Unit = {
    val testSmiles = "C2O6H2Cl4"
//    println(Utils.getOccurences(testSmiles) mkString "\n")
    val testSmiles2 = "C2O6H2Cl4P5"
//    println(Utils.getOccurences(testSmiles2) mkString "\n")

    val testSmiles3 = "C2H2I2"
    println(Utils.getOccurences(testSmiles3) mkString "\t")
  }

  @Test def makeArray(): Unit = {
    def getLength(idx: Int): Int = Utils.makeAllPossibilityArray(idx).length

    assertTrue(getLength(3) == 64)
    assertTrue(getLength(2) == 9)
  }

  @Test def partitionTest(): Unit = {
    val testSmiles = "C2H2I2"
    val result = Utils.partition(testSmiles)
    val expectation = Array(Array(0, 2))
    assertEquals(result(0)(0), expectation(0)(0))
    assertEquals(result(0)(1), expectation(0)(1))

    val testSmiles2 = "C2O6H2"
    val result2 = Utils.partition(testSmiles2)
    val expectation2 = Array(Array(2, 0), Array(1, 1), Array(0, 2))
    assertEquals(result2(0)(0), expectation2(0)(0))
    assertEquals(result2(0)(1), expectation2(0)(1))
    assertEquals(result2(1)(0), expectation2(1)(0))
    assertEquals(result2(1)(1), expectation2(1)(1))
    assertEquals(result2(2)(0), expectation2(2)(0))
    assertEquals(result2(2)(1), expectation2(2)(1))
  }

  @Test def getValences(): Unit = {
    val testSmiles = "C2H2I2"
    val expectedList = Array(0, 3, 0)
    val resultArr = Utils.getValences(testSmiles)
    assertArrayEquals(expectedList, resultArr)
  }
}