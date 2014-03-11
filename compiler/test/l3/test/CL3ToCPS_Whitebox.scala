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
    
  @Test def testTailLetRec3 =
    testCPSHighTreeEquality("(letrec ((f (fun (g) (@+ g 2)))) f)", "(let ((v$1 (fun (v$2 v$3) (let* ((v$4 2) (v$5 (+ v$3 v$4))) (v$2 v$5))))) (halt))")
  
  @Test def testNonTailMultiLetRecAndApp =
    testCPSHighTreeEquality("((fun (x y) y) 1 2)",
        "(let* ((v$1 (fun (v$2 v$3 v$4) (v$2 v$4))) (v$5 1) (v$6 2) (v$7 (cnt (v$8) (halt)))) (v$1 v$7 v$5 v$6))")
  
  @Test def testTailIfInsideLetRec =
    testCPSHighTreeEquality("(fun (x y) (if #t x y))",
        "(let ((v$1 (fun (v$2 v$3 v$4)" +
          "(let* ((v$5 (cnt () (v$2 v$3)))" +
                 "(v$6 (cnt () (v$2 v$4)))" +
                 "(v$7 #t)" +
                 "(v$8 #f))" +
               "(if (!= v$7 v$8) v$5 v$6))))) (halt))")
               
  @Test def testCondNestedExprFalse =
    testCPSHighTreeEquality("(if (if (@ = 3 4) (if (@ = 3 4) #t #f) #f) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 (cnt () (let* ((v$8 3) (v$9 4)) (if (= v$8 v$9) v$3 v$5))))" +
             "(v$10 3)" +
             "(v$11 4))" +
          "(if (= v$10 v$11) v$7 v$5))")
  // TODO: Add more tests
    
}
