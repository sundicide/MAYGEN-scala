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

  def getFuzzyFormulaRanges(localFormula: String): Unit = {
    val atoms: Array[String] = localFormula.split(Consts.LETTERS_FROM_A_TO_Z)
    val atom: Array[Array[String]] = atoms.map((atom: String) => atom.split("\\["))
    atom foreach (elem => println(elem mkString (", ")))
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
}
