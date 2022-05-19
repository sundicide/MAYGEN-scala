package MAYGEN

import org.apache.commons.lang3.StringUtils

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Utils {
  /**
   * param내 '(' 글자를 제거한다.
   * @param paramFormula 타겟 string
   * @return 제거한 결과
   */
  def checkFormula(paramFormula: String): String = {
    paramFormula.replace("(", "")
  }

  def checkLengthTwoFormula(atoms: Array[String]): Boolean = {
    if (atoms.length == 1) {
      val info = atoms.head.split(Consts.NUMBERS_FROM_0_TO_9, 2)
      if (atoms.head.contains("(")) {
        val info2 = info(1).split("\\)")
        if (info(1).equals("2") && Integer.valueOf(info.head) > 3) {
          return false
        }
      } else {
        val getValue: Int = Consts.VALENCE.getOrElse(info.head, 0)
        if (info(1).equals("2") && getValue > 3) {
          return false
        }
      }
    }
    return true
  }

  /**
   * getValences로 구해진 결과 값들에 대해 각 원소의 valence 값을 곱한다.
   * @param formula
   * @return
   */
  def getCapacity(formula: String): Array[Int] = {
    val atomsHashMap = Utils.makeAtomHashMap(formula)
    val atomList = sortAtomListDesc(formula)
      .map(d => atomsHashMap.getOrElse(d, -1))
    getValences(formula)
      .zipWithIndex
      .map(d => {
        val calcResult = d._1 * atomList(d._2)
        if (calcResult < 0 ) 0
        else calcResult
      })
  }

  /**
   * formula의 degree List를 계산한다
   * @param formula C2O6H2
   * @return [4, 4, 2, 2, 2, 2, 2, 2, 1, 1]
   */
  def getDegrees(formula: String): Array[Int] = {
    val symbolList: Array[String] = getOnlySymbols(formula)
    initDegrees(symbolList)
  }

  def getFuzzyFormulaRanges(localFormula: String): Unit = {
    val atoms: Array[String] = localFormula.split(Consts.LETTERS_FROM_A_TO_Z)
    val atom: Array[Array[String]] = atoms.map((atom: String) => atom.split("\\["))
    atom foreach (elem => println(elem mkString (", ")))
  }

  /**
   * formula의 occurences List를 계산한다.
   * @param formula C2O6H2
   * @return [2, 6, 2]
   */
  def getOccurences(formula: String): Array[Int] = {
    val atomHashMap: HashMap[String, Int] = Utils.makeAtomHashMap(formula)
    sortAtomListDesc(formula)
      .map(atom => atomHashMap.getOrElse(atom, -1))
  }

  /**
   * Atom을 동일 atom 등장 개수, valence 크기에 따라 정렬한 뒤 각 값에 맞게 계산한다.
   * @param formula C2I2H2
   * @return [0, 3, 0]
   *   앞에서부터 차례로 [I, C, H] 이다.
   *   H는 항상 뒤에, I와 C는 개수가 동일하므로 valence 값이 더 큰 C가 뒤에 배치
   *   또한 I의 Valence값은 1이므로 1-1 = 0이 되고
   *   C의 Valence값은 4이므로 4-1 = 3이 된다.
   */
  def getValences(formula: String): Array[Int] = {
    sortAtomListDesc(formula)
      .map(d => {
        val valence = Consts.VALENCE.getOrElse(d, 0) - 1
        if (valence < 0) 0
        else valence
      })
  }

  def normalizeFormula(param: String): String = {
    val exchanger: Map[Char, Char] = Map(
      'c' -> 'C',
      'L' -> 'l',
      'n' -> 'N',
      'o' -> 'O',
      's' -> 'S',
      'p' -> 'P',
      'f' -> 'F',
      'i' -> 'I',
      'b' -> 'B',
      'R' -> 'r',
      'h' -> 'H'
    )
    param.map((chr: Char) => {
      val found = exchanger.keys.find(k => k == chr)
      found match {
        case Some(d: Char) => exchanger.get(d) match {
          case Some(result) => result
          case _ => ' '
        }
        case _ => chr
      }
    })
  }

  def validateFormula(formula: String): Array[String] = {
    val from = Array("Cl", "C", "N", "O", "S", "P", "F", "I", "Br", "H")
    val to = Array("", "", "", "", "", "", "", "", "", "")

    val replaceResults = StringUtils.replaceEach(
      formula
        .replaceAll("[0-9]", ""),
      from,
      to
    )
    if (replaceResults.isEmpty) return Array()
    else replaceResults.split("")
  }

  def splitByAtoms(formula: String): Array[String] = formula.split(Consts.LETTERS_FROM_A_TO_Z)
  def splitByNumbers(formula: String): Array[String] = formula.split(Consts.NUMBERS_FROM_0_TO_9)

  def sum(list: Array[Int], index: Int): Int = list.slice(0, index + 1).sum

  /**
   * Formula로부터 Hydrogen 갯수를 반환한다.
   * 예) C2H6O2H10 -> 16(6+10)
   * @param formula 구조식(eg. C6H2O5)
   */
  def getHydrogensCount(formula: String): Int = {
    val atoms: Array[String] = splitByAtoms(formula)
    atoms.filter((atom: String) => atom.head.equals('H'))
      .foldLeft(0)((acc, curr) => acc + new Integer(splitByNumbers(curr).tail.reduceLeft(_ + _)))
  }

  /**
   * 받은 formula로부터 갯수만큼 원소로 채운 배열을 리턴한다.
   * @param formula
   * @return 원소 배열
   *         예) C2H2O2 -> ["C", "C", "H", "H", "O", "O"]
   */
  def getOnlySymbols(formula: String): Array[String] = {
    val atoms: Array[String] = splitByAtoms(formula)

    val result = atoms.foldLeft(Array(): Array[String])((accu, curr) => {
      val splittedAtom = splitByNumbers(curr)
      val atomChar = splittedAtom.head
      val count = new Integer(splittedAtom.tail.reduceLeft(_ + _))

      @tailrec
      def loop(num: Int, acc: Array[String]): Array[String] = {
        if (num == 0) acc
        else loop(num - 1, atomChar +: acc)
      }
      val atomChars: Array[String] = loop(count, Array())
      accu ++ atomChars
    })
    result
  }

  /**
   * 원소기호와 숫자로 이루어진 string을 원소기호의 반복으로 나타낸다.
   * @param atom "C2"
   * @return ["C", "C"]
   */
  def makeOneAtomSerialize(atom: String): List[String] = {
    val splittedAtom = splitByNumbers(atom)
    def loop(num: Int, acc: List[String]): List[String] = {
      if (num == 0) acc
      else {
        loop(num - 1, acc :+ splittedAtom(0))
      }
    }
    loop(splittedAtom(1).toInt, List())
  }

  /**
   * 전달반은 smiles로 HashMap을 만든다.
   * @param smilesString "C2O6C2"
   * @return HashMap("C" -> 4, "O" -> 6)
   */
  def makeAtomHashMap(smilesString: String): HashMap[String, Int] = {
    @tailrec
    def loop(atoms: Array[String], accu: HashMap[String, Int]): HashMap[String, Int] = {
      if (atoms.length == 0) accu
      else {
        val head = atoms.head
        val headAtom = splitByNumbers(head)(0)
        val headNumber: Int = splitByNumbers(head)(1).toInt
        val found = accu.keys.find(key => headAtom.equals(key))
        found match {
          case Some(d) => {
            val kk = accu.getOrElse(d, 0)
            val newKK = kk + headNumber
            loop(atoms.tail, accu + (d -> newKK))
          }
          case _ => {
            loop(atoms.tail, accu + (headAtom -> headNumber))
          }
        }
      }
    }
    loop(splitByAtoms(smilesString), HashMap())
  }

  /**
   * 전달 받은 atomList를 각 Valence 값으로 치환한다.
   * @param atomList ["C", "C", "O", "O"]
   * @return [4, 4, 2, 2]
   */
  def initDegrees(atomList: Array[String]): Array[Int] = {
    atomList.map(atom => Consts.VALENCE.getOrElse(atom, -1))
  }

  def distributeHydrogens(formula: String): Unit = {
    // 알 수 없는 값
    var callHydrogenDistributor = true

    if (!callHydrogenDistributor) {
      // firstDegrees
    } else {
      val distributions = HydrogenDistributor.run(formula)
    }
  }

  /**
   * 주어진 index 만큼의 배열내 배열을 생성한다.
   * @param idx 2
   * @return [[0,2], [1,2], [2,2], ...]
   */
  def makeAllPossibilityArray(idx: Int): Array[Array[Int]] = {
    @tailrec
    def loop(arr: Array[Array[Int]]): Array[Array[Int]] = {
      if (arr.head.length >= idx) arr
      else {
        val result = for {
          ar <- arr
          i <- 0 to idx
        } yield(ar :+ i)
        loop(result)
      }
    }
    loop(Array(Array()))
  }

  /**
   * 아직은 어떤 기능인지 알 수는 없지만 본 코드에서 필요한 부분이라 생각해 가져온 함수
   * 본 코드에서는 getPartition이란 네이밍이다.
   * @param formula C2H6O2
   * @return [[2, 0], [1, 1], [0, 2]]
   */
  def partition(formula: String): Array[Array[Int]] = {
    val degrees = Utils.getOccurences(formula)
    val hydrogenCounts = Utils.getHydrogensCount(formula)
    val isotopes = degrees.length - 1
    val capacity = Utils.getCapacity(formula)

    val allArr = makeAllPossibilityArray(isotopes)
    allArr.filter((d: Array[Int]) => {
      val innerArr: Array[Int] = d
      if (innerArr.sum != hydrogenCounts) false
      else {
        val found = innerArr.zipWithIndex.find(d => d._1 > capacity(d._2))
        found match {
          case Some(_) => false
          case _ => true
        }
      }
    })
      .reverse
  }

  /**
   * Atom을 개수, valence 크기에 따라 정렬한다.
   * @param formula C2I2H2
   * @return [I, C, H]
   *   H는 항상 뒤에, I와 C는 개수가 동일하므로 valence 값이 더 큰 C가 뒤에 배치
   */
  def sortAtomListDesc(formula: String): Array[String] = {
    val atomHashMap: HashMap[String, Int] = Utils.makeAtomHashMap(formula)
    val atomCountTupleArray: Array[(String, Int)] = atomHashMap.toArray

    val (nonHydrogenTupleList, hydrogenTuple) = atomCountTupleArray.foldLeft(Array(): Array[(String, Int)], null: (String, Int))((acc, curr) => {
      curr match {
        case (key: String, _: Int) => if (key.equals("H")) (acc._1, curr)
        else (acc._1 :+ curr, acc._2)
      }
    })
    val sortedArray = nonHydrogenTupleList.sortWith((before, after) => {
      val (beforeAtom, beforeValue) = before
      val (afterAtom, afterValue) = after

      if (beforeValue == afterValue) {
        Consts.VALENCE.getOrElse(afterAtom, -1) - Consts.VALENCE.getOrElse(beforeAtom, -1) > 0
      } else {
        afterValue - beforeValue > 0
      }
    })
      .map(d => d._1)
    sortedArray :+ hydrogenTuple._1
  }
}
