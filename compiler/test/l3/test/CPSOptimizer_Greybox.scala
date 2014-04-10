package l3
package test

import org.junit.Test
import l3.test.infrastructure.CPSOptTest

/** Greybox testing for entire program outputs:
 *    - much like blackbox tests, but can access the internal state of the interpreter
 *    - they can query how many instructions or primitives have been executed for example */
class CPSOptimizer_Greybox extends CPSOptTest {

  // example tests:

  @Test def testDCEFunsSimple =
    testCPSOptEarly("(letrec ((f (fun (x) (f x)))) #u)", _.getFuncs == 0)

  @Test def testConstantFoldingIntP =
    testCPSOptLate("(let ((x (@ int? 1))) #u)", stats => stats.get(LetP) == 0 && stats.get(If) == 0)

  @Test def testCommonSubexpressionEliminationSimpleBlockGet =
    testCPSBothSeq("(let ((b (@ block-alloc-3 2))) (@ char-print (@ int->char (@ + (@ block-tag b) (@ block-tag b)))))", _.get(BlockTag) == 1)

  @Test def testConstantFoldingPlus =
    testCPSBothPar("(@ + 2 1)", stats => stats.get(LetP) == 0)
}
