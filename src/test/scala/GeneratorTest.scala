import MAYGEN.{Generation, HydrogenDistributor, Utils}
import org.junit.Assert.assertEquals
import org.junit.Test

class GeneratorTest {
  trait TestSets {
    val testSmiles = "c2O6H2"
    val testSmiles2 = "C6H6"
    val normalizedSmiles = Utils.normalizeFormula(testSmiles)
    val normalizedSmilesAtoms = Array("C2", "O6", "H2")

    val abnormalSmiles = "asdqwe"
    val normalizedAbnormalSmiles = Utils.normalizeFormula(abnormalSmiles)
  }

  @Test def `setHydrogens`() : Unit = {
    new TestSets {
      val degrees = List(4, 2, 2, 2, 2, 2, 2, 2)
      val result = Generation.setHydrogens(degrees, normalizedSmiles)
      assertEquals(
        List(0, 2, 0, 0, 0, 0, 0, 0),
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
