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

  @Test def testTailLetRec =
    testCPSHighTreeEquality("(letrec ((f (fun (g) (g)))) f)", "(let ((v$1 (fun (v$2 v$3) (v$3 v$2)))) (halt))")
    
  @Test def testTailLetRec2 =
    testCPSHighTreeEquality("(letrec ((f (fun (g) (g 2)))) f)", "(let ((v$1 (fun (v$2 v$3) (let ((v$4 2)) (v$3 v$2 v$4))))) (halt))")
  
  @Test def testGCD = {
    testCPSHighTreeEquality("(rec loop ((x 2016) (y 714)) (if (@= 0 y) (@char-print 'A') (loop y (@% x y))))",
        "(letc ((loop (cnt (x y) (let* ((ct (cnt () (appf print x))) (cf (cnt () (letp ((t (% x y))) (appc loop y t)))) (z 0)) (if (= y z) ct cf))))) (let* ((x 2016) (y 714)) (appc loop x y)))")
  }
    
    
    // TODO: Add more tests
    
}
