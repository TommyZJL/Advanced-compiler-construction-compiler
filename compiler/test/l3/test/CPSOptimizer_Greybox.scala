package l3
package test

import org.junit.Test
import l3.test.infrastructure.CPSOptTest

/** Greybox testing for entire program outputs:
 *    - much like blackbox tests, but can access the internal state of the interpreter
 *    - they can query how many instructions or primitives have been executed for example */
class CPSOptimizer_Greybox extends CPSOptTest {

  // example tests:
  @Test def testBasic = 
    testCPSOptEarly("(let ((x 1)) x)", _.get(LetL) == 0)
  
  @Test def testDCEFunsSimple =
    testCPSOptEarly("(letrec ((f (fun (x) (f x)))) #u)", _.getFuncs == 0)

  @Test def testConstantFoldingIntP =
    testCPSOptLate("(let ((x (@ int? 1))) #u)", stats => stats.get(LetP) == 0 && stats.get(If) == 0)

  @Test def testCommonSubexpressionEliminationSimpleBlockGet =
    testCPSBothSeq("(let ((b (@ block-alloc-3 2))) (@ char-print (@ int->char (@ + (@ block-tag b) (@ block-tag b)))))", _.get(BlockTag) == 1)

  @Test def testConstantFoldingPlus =
    testCPSBothPar("(@ + 2 1)", stats => stats.get(LetP) == 0)
    
  @Test def testConstantFoldingMinus =
    testCPSBothPar("(@ - 2 1)", stats => stats.get(LetP) == 0)
    
  @Test def testConstantFoldingTimes =
    testCPSBothPar("(@ * 2 1)", stats => stats.get(LetP) == 0)
    
  @Test def testConstantFoldingDiv =
    testCPSBothPar("(@ / 2 1)", stats => stats.get(LetP) == 0)
  
  @Test def testConstantFoldingAnd =
    testCPSBothPar("(if (@ & #f #t) (@char-print 'a') (@char-print 'b'))", stats => false)
  
  
  @Test def testFunInlining =
    testCPSBothSeq("(def f (fun (x) (@ char-print 'a'))) (f 1)", s => s.get(LetF) == 0)
    
  @Test def testNeutral =
    testCPSBothSeq("(let ((a (@char-read))) (@char-print (@+ 0 a)))", s => s.get(LetL) == 2)
    
  @Test def testInliningConstantFoldingDCE =
    testCPSBothSeq("(let* ((a 1) (b 2) (c (@ + a b))) (@ char-print 'a'))", s => s.get(LetL) == 1)
    
}
