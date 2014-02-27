package l3.test

import l3.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CL3ToCPS_Whitebox extends CPSHighTest {

  @Test def testNonTailLiteral =
    testCPSHighTreeEquality("3", "(let ((v$1 3)) (halt))")

  @Test def testNonTailMultiLet =
    testCPSHighTreeEquality("(let ((x 1) (y 2)) y)",
        "(let* ((v$1 (cnt (v$2) (let* ((v$3 (cnt (v$4) (halt))) (v$5 2)) (v$3 v$5)))) (v$6 1)) (v$1 v$6))")

  // TODO: Add more tests
}
