import MAYGEN.Utils
import org.junit.Test
import org.junit.Assert._

class HydrogenDistributorTest {
  trait TestSets {
    val testSmiles = "c2O6H2"
    val normalizedSmiles = Utils.normalizeFormula(testSmiles)
    val normalizedSmilesAtoms = Array("C2", "O6", "H2")

    val abnormalSmiles = "asdqwe"
    val normalizedAbnormalSmiles = Utils.normalizeFormula(abnormalSmiles)
  }
  @Test def hydrogenDistributePrepareTest(): Unit = {
    /**
     * 모든 경우의 수에 해당하는 배열을 생성한다.
     * @param arrSize 생성될 배열의 사이즈
     * @param maxValue 각 요소에 할당할 수 있는 최대 값
     * @return 생성된 배열 List
     */
    def makeArray(arrSize: Int, maxValue: Int) = {
      val initList: List[List[Int]] = (0 to maxValue).toList map((d: Int) =>  List(d))

      def iter(p: List[List[Int]]) = {
        for {
          pi <- p
          i <- 0 to maxValue
        } yield (i :: pi)
      }

      def doIter(count: Int, currentList: List[List[Int]]): List[List[Int]] = {
        if (count < arrSize - 1) doIter(count + 1, iter(currentList))
        else currentList
      }
      doIter(0, initList)
    }

    assertEquals(
      List(List(0, 0), List(1, 0), List(2, 0), List(0, 1), List(1, 1), List(2, 1), List(0, 2), List(1, 2), List(2, 2)),
      makeArray(2, 2)
    )

    /**
     *
     * @param arrSize 배열 길이
     * @param maxValue 배열내 원소의 최대 값
     * @param totalValue 배열 내 모든 원소의 총합 기준
     *              예) total=2 라면 배열 내 모든 원소의 합이 2여야 한다.
     * @return
     */
    def filterByTotalValue(arrSize: Int, maxValue: Int, totalValue: Int): List[List[Int]] = {
      makeArray(arrSize, maxValue).filter((d: List[Int]) => d.sum <= totalValue)
    }

    /**
     * 맨 뒤에서 부터 값이 채워진 배열 리턴
     * @param arrSize 배열 길이
     * @param maxValue 배열내 원소의 최대 값
     * @param totalValue 배열 내 모든 원소의 총합 기준
     *              예) total=2 라면 배열 내 모든 원소의 합이 2여야 한다.
     * @return
     */
    def sortArray(arrSize: Int, maxValue: Int, totalValue: Int) = {
      filterByTotalValue(arrSize, maxValue, totalValue).map(_.sorted)
        .distinct
    }

    val total = 6
    val targetList = List(
      List(6, 3, total)
    )

    val mapped = targetList.map(d => {
      sortArray(d(0), d(1), d(2))
    })

    val temp = mapped.reduceLeft((a, b) => {
      for {
        aa <- a
        bb <- b
      } yield aa.concat(bb)
    })
    val result = temp.filter(d => d.sum == total)
    assertEquals(
      List(List(0, 0, 0, 0, 3, 3), List(0, 0, 0, 1, 2, 3), List(0, 0, 0, 2, 2, 2), List(0, 0, 1, 1, 1, 3), List(0, 0, 1, 1, 2, 2), List(0, 1, 1, 1, 1, 2), List(1, 1, 1, 1, 1, 1)),
      result
    )
  }

  @Test def `zipTest`() : Unit = {
  }
}
