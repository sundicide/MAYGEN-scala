import MAYGEN.{HydrogenDistributor, Utils}
import org.junit.Test
import org.junit.Assert._

class HydrogenDistributorTest {
  trait TestSets {
    val testSmiles = "C2O6H2"
    val testSmiles2 = "C6H6"
    val normalizedSmiles = Utils.normalizeFormula(testSmiles2)
    val normalizedSmilesAtoms = Array("C2", "O6", "H2")

    val abnormalSmiles = "asdqwe"
    val normalizedAbnormalSmiles = Utils.normalizeFormula(abnormalSmiles)
  }

  @Test def `newDegrees`() : Unit = {
    new TestSets {
      assertEquals(
        List(List(4, 4, 2, 2, 2, 2, 1, 1), List(4, 3, 2, 2, 2, 2, 2, 1), List(4, 2, 2, 2, 2, 2, 2, 2), List(3, 3, 2, 2, 2, 2, 2, 2)),
        HydrogenDistributor.newDegrees(testSmiles)
      )
    }
  }

  @Test def `newPartition`() : Unit = {
    new TestSets {
      val result = HydrogenDistributor.newPartition(testSmiles)
      val occur = Utils.getOccurences(testSmiles)
      assertEquals(
        List(List(2, 4, 2), List(1, 1, 5, 1), List(1, 1, 6), List(2, 6)),
        result
      )
    }
  }
}
