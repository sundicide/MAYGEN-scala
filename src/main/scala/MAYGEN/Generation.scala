package MAYGEN

import scala.collection.immutable.HashMap

object Generation {

  def structureGenerator(localFormula: String): Unit = {
    var noHydrogen: Boolean = false
    var justH: Boolean = false
    var hIndex: Int = -1
    var size: Int = -1

    if (noHydrogen) { // "H" 가 없을 때?
      size = Utils.getOnlySymbols(localFormula).length - Utils.getHydrogensCount(localFormula)
    } else if (justH) { // 오직 "H"만 있을 때?
      size = Utils.getOnlySymbols(localFormula).length - Utils.getHydrogensCount(localFormula)
    } else { // 원소기호 "H" 를 제외한 나머지 원소들의 개수
      size = Utils.getOnlySymbols(localFormula).length - Utils.getHydrogensCount(localFormula)
    }
      val newDegrees = Utils.distributeHydrogens(localFormula)
      val atoms: Array[String] = Utils.splitByAtoms(localFormula)
      val atomHashMap: HashMap[String, Int] = Utils.makeAtomHashMap(localFormula)
      val atomTupleList = atomHashMap.toList
      val (nonHydrogenTupleList, hydrogenTuple) = atomTupleList.foldLeft(List(): List[(String ,Int)], null: (String, Int))((acc: (List[(String, Int)], (String, Int)), curr: (String, Int)) => {
        curr match {
          case (key: String, value: Int) => if (key.equals("H")) (acc._1, curr)
          else (acc._1 :+ curr, acc._2)
        }
      })
      val sorted = nonHydrogenTupleList.sortBy(d => d._2)
      val result = sorted.flatMap(d => Utils.makeOneAtomSerialize(d._1 + d._2.toString))
      val newResult = result :+ Utils.makeOneAtomSerialize(hydrogenTuple._1 + hydrogenTuple._2.toString)
      // MAYGEN.java 569라인 symbolList.add("H")까지 ==========


      // MAYGEN.java 570라인 getPartition 부분
      val firstOccurences = sorted.map(tuple => tuple._2) :+ hydrogenTuple._2
      println(s"firstOccurences = ${firstOccurences}")

      // MAYGEN.java 572라인 setSymbols
      val matrixSize = firstOccurences.sum
      println(s"matrixSize = ${matrixSize}")

      // MAYGEN.java 573라인 getParition은 제외
      val hydrogenCount = hydrogenTuple._2
      val occurences = Utils.partition(localFormula)
      // 아직은 알 수 없는 변수들
      var calllHydrogenDistributor = false
      justH = false
      noHydrogen = true

      if (hydrogenCount != 0) {
        val hydrogenIndex = sorted.map(tuple => tuple._2).sum
        if (hydrogenIndex == 1) {
          calllHydrogenDistributor = false
        } else if (hydrogenIndex == 0) {
          justH = true
          calllHydrogenDistributor = false
        } else {
          calllHydrogenDistributor = true
        }
      } else {
        calllHydrogenDistributor = false
        noHydrogen = true
      }
    }
}
