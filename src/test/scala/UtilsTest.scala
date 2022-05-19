import MAYGEN.Utils
import org.junit.Assert._
import org.junit._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

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
    println(Utils.getValences("C2O6H2") mkString "\t")
  }

  @Test def hydrogenDistributePrepareTest(): Unit = {
    val n = 3 // [0,0,3] ... [3,0,0] 27개의 배열을 생성

    def step1(n: Int, max: Int) = {
      val initList: List[List[Int]] = (0 to max).toList map((d: Int) =>  List(d))

      def iter(p: List[List[Int]]) = {
        for {
          pi <- p
          i <- 0 to max
        } yield (i :: pi)
      }

      def doIter(count: Int, currentList: List[List[Int]]): List[List[Int]] = {
        if (count < n - 1) doIter(count + 1, iter(currentList))
        else currentList
      }
      doIter(0, initList)
    }

    /**
     *
     * @param n 배열 길이
     * @param max 배열내 원소의 최대 값
     * @param total 배열 내 모든 원소의 총합 기준
     *              예) total=2 라면 배열 내 모든 원소의 합이 2여야 한다.
     * @return
     */
    def step2(n: Int, max: Int, total: Int): List[List[Int]] = {
      step1(n, max).filter((d: List[Int]) => d.sum <= total)
    }
//    assertEquals(
//      List(List(1, 1, 0), List(1, 0, 1), List(0, 1, 1)),
//      step2(3, 1, 2)
//    )

    /**
     * 맨 뒤에서 부터 값이 채워진 배열 리턴
     * @param n
     * @param max 배열내 원소의 최대 값
     * @param total
     * @return
     */
    def step3(n: Int, max: Int, total: Int) = {
      step2(n, max, total).map(d => d.sorted)
        .distinct
    }
//    assertEquals(
//      List(List(1, 0, 1), List(0, 1, 1), List(0, 0, 2)),
//      step3(3, 2, 2)
//    )

    val c = step3(2, 3, 2)
    val o = step3(6, 1, 2)
    val k = step3(4, 2, 2)

//    val total = 2
//    val targetList = List(
//      List(2, 3, total),
//      List(2 , 1, total),
//      List(2, 2, total)
//    )
    val total = 6
    val targetList = List(
      List(6, 3, total)
    )

    val mapped = targetList.map(d => {
      step3(d(0), d(1), d(2))
    })
    val temp = mapped.reduceLeft((a, b) => {
      for {
        aa <- a
        bb <- b
      } yield aa.concat(bb)
//      s.filter(d => d.sum == total)
    })
    val result = temp.filter(d => d.sum == total).distinct
    val result2 = temp.filter(d => d.sum == total)
    println(result mkString ", ")
    println(result2.size)

//    val s = for {
//      cc <- c
//      oo <- o
//    } yield cc ::: oo
//    val result = s.filter(d => d.sum == 2)
//    println(result mkString ", ")
  }
}