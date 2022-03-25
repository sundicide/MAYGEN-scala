package MAYGEN

import scala.collection.immutable.HashMap

object Consts {
  val LETTERS_FROM_A_TO_Z = "(?=[A-Z])"
  val NUMBERS_FROM_0_TO_9 = "(?=[0-9])"
  /**
   * CDK로부터 온 atom valences
   */
  val VALENCE = HashMap[String, Int](
    "C" -> 4,
    "N" -> 3,
    "O" -> 2,
    "S" -> 2,
    "S" -> 2,
    "P" -> 3,
    "F" -> 1,
    "I" -> 1,
    "Cl" -> 1,
    "Br" -> 1,
    "H" -> 1
  )
}
