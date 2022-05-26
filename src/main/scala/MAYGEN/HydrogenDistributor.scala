package MAYGEN

object HydrogenDistributor {
  def run(formula: String): Unit = {
    val occurences = Utils.getOccurences(formula)
    val degrees = Utils.getDegrees(formula)

    val partitionSize = occurences.length
    val hydrogen = occurences.last
    val isotopes = partitionSize - 1
    val localCapicity = setValues(occurences, degrees)
    val totalHydrogen = Utils.getHydrogensCount(formula)
    if (isotopes == 1) {
      val hydrogen2distribute = totalHydrogen
    } else {
      val partitionResult: Array[Array[Int]] = Utils.partition(formula)
    }
  }

  def setValues(occurences: Array[Int], degrees: Array[Int]): Array[Int] = {
    occurences.map((occur: Int) => {
      val localValences = degrees(occur-1) - 1
      localValences * occur
      }
    )
  }

  def distribute(formula: String): List[List[Int]] = {
    val valences = Utils.getValences(formula)
    val occurrences = Utils.getOccurences(formula)
    val hydrogenCount = Utils.getHydrogensCount(formula)

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

    val result = (valences zip occurrences).init

    result
      .map({
      case (valence: Int, occurs: Int) => sortArray(occurs, valence, hydrogenCount)
      })
      .reduceLeft((a, b) => {
        for {
          aa <- a
          bb <- b
        } yield aa.concat(bb)
      })
      .filter(d => d.sum == hydrogenCount)
  }

  def newDegrees(formula: String): List[List[Int]] = {
    val valences = Utils.getValences(formula)
    val occurrences = Utils.getOccurences(formula)

    val result = (valences zip occurrences).init

    val distributed = distribute(formula)

    val tt = result.toList.foldLeft(List(0))((acc, curr) => {
      def append(targetArr: List[Int], idx: Int): List[Int] = {
        if (idx == 0) targetArr
        else append((curr._1 + 1) :: targetArr, idx - 1)
      }
      append(acc, curr._2)
    }).init.reverse

    distributed.map(d => {
      d.zipWithIndex.map(dd => tt(dd._2) - dd._1)
    })
  }

  def newPartition(formula: String): List[List[Int]] = {
    newDegrees(formula).map(f => {
      f.foldLeft(Nil: List[(Int, Int)])((accu, curr) => {
        if (accu.isEmpty) List((curr, 1))
        else {
          if (curr == accu.head._1) (curr, accu.head._2 + 1) :: accu.tail
          else (curr, 1) :: accu
        }
      }).reverse.map(d => d._2)
    })
  }
}
