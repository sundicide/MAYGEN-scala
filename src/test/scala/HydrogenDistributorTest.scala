import MAYGEN.{HydrogenDistributor, Utils}
import org.junit.Test
import org.junit.Assert._

class HydrogenDistributorTest {
  trait TestSets {
    val testSmiles = "c2O6H2"
    val testSmiles2 = "C6H6"
    val normalizedSmiles = Utils.normalizeFormula(testSmiles)
    val normalizedSmilesAtoms = Array("C2", "O6", "H2")

    val abnormalSmiles = "asdqwe"
    val normalizedAbnormalSmiles = Utils.normalizeFormula(abnormalSmiles)
  }

  @Test def `newDegrees`() : Unit = {
    new TestSets {
      val result = HydrogenDistributor.newDegrees(normalizedSmiles)
      assertEquals(
        List(List(4, 4, 2, 2, 2, 2, 1, 1), List(4, 3, 2, 2, 2, 2, 2, 1), List(4, 2, 2, 2, 2, 2, 2, 2), List(3, 3, 2, 2, 2, 2, 2, 2)),
        result
      )
    }
  }

  @Test def `newPartition`() : Unit = {
    new TestSets {
      val result = HydrogenDistributor.newPartition(normalizedSmiles)
      assertEquals(
        List(List(2, 4, 2), List(1, 1, 5, 1), List(1, 7), List(2, 6)),
        result
      )
    }
  }
}
