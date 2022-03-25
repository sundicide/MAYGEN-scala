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
}
