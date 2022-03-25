package MAYGEN

import org.openscience.cdk.interfaces._
import org.openscience.cdk
import org.openscience.cdk.DefaultChemObjectBuilder
import org.openscience.cdk.smiles.{SmiFlavor, SmilesGenerator}


object MAYGEN {
  private final val builder: IChemObjectBuilder = DefaultChemObjectBuilder.getInstance
  private val smilesGenerator = new SmilesGenerator(SmiFlavor.Unique)
  private var atomContainer = builder.newInstance(classOf[IAtomContainer])

  // The atom valences from CDK
  val valences: Map[String, Int] = Map(
    "C" -> 4,
    "N" -> 3,
    "O" -> 2,
    "S" -> 2,
    "P" -> 3,
    "F" -> 1,
    "I" -> 1,
    "Cl" -> 1,
    "Br" -> 1,
    "H" -> 1
  )

  def splitSmiles(atoms: String): Array[String] = {
    atoms.split("(?=[A-Z])")
  }

  def checkLengthTwoFormula(atoms: Array[String]): Boolean = {
    if (atoms.length != 1) false
    else {
      val info = atoms.head.split("(?=[0-9])", 2)
      if (info(1).equals("2") && valences.getOrElse(info(0), -1) > 3) true
      else false
    }
  }


  def main(args: Array[String]): Unit = {
    val testCode = "c2O6H2"
    val arachidonicAcid = "CCCCCC=CCC=CCC=CCC=CCCCC(=O)O"
    val normalizedFormula = Utils.normalizeFormula(testCode)
    println("normalizedFormula", normalizedFormula)
    val checkedFormula = Utils.checkFormula(normalizedFormula)
    println(s"checkedFormula: $checkedFormula")
    val isValid = Utils.validateFormula(checkedFormula)
    if (!isValid.isEmpty) {
      println("The input formula consists user defined element types: " + (isValid mkString (", ")))
      return
    }
    val atoms: Array[String] = Utils.splitByAtoms(checkedFormula)
    if (Utils.checkLengthTwoFormula(atoms)) {}
    //    Utils.getFuzzyFormulaRanges(normalizedFormula)
    Generation.structureGenerator(normalizedFormula)

    Utils.initDegrees(Utils.getOnlySymbols(normalizedFormula))

    Generation.structureGenerator(normalizedFormula)
  }
}