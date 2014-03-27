package l3.test

import l3.test.infrastructure.CPSLowTest
import org.junit.Test
import l3.test.ok.AllOKTests

/** Blackbox testing for entire program outputs */
class CPSDataRepresentation_Blackbox extends CPSLowTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSLowProgramOutput(source = src)
  // TODO: Add other specific tests here
  
  @Test def onePlusTwo = compileAndInterpret("(@ + 1 2)")
}
