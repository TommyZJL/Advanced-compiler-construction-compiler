package l3.test

import l3.test.infrastructure.CPSLowTest
import org.junit.Test
import l3.test.ok.AllOKTests
import l3.MainHelper

/** Blackbox testing for entire program outputs */
class CPSDataRepresentation_Blackbox extends CPSLowTest with AllOKTests with MainHelper {

  
  lazy val library: String = {
    val fs = java.nio.file.FileSystems.getDefault
    val inFiles = expandModules(fs.getPath(""), Seq("../library/lib.ml3")).distinct
    val inSource = inFiles.map({ f => scala.io.Source.fromFile(f.toString).mkString }).mkString("\n")
    inSource
  }
  def compileAndInterpretWithLib: String => Unit = (source: String) => compileAndInterpret(library + "\n" + source)

  @Test def testLibFunctions1 =
    compileAndInterpretWithLib("""
      (char-print (if (function? function?) 'O' 'K'))
      (char-print (if (function? 42) 'O' 'K'))
      (newline-print)
    """)
    
  val compileAndInterpret = (src: String) => testCPSLowProgramOutput(source = src)
  // TODO: Add other specific tests here
}
