package l3.test

import l3.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CL3ToCPS_Whitebox_Tail extends CPSHighTest {

  @Test def testTailUselessContinuations =
    testCPSHighTreeEquality("(letrec ((f (fun (g) (g)))) f)", "(let ((v$1 (fun (v$2 v$3) (v$3 v$2)))) (halt))")

  @Test def testTailIfInsideLetRec =
    testCPSHighTreeEquality("(fun (x y) (if #t x y))",
        "(let ((v$1 (fun (v$2 v$3 v$4)" +
          "(let* ((v$5 (cnt () (v$2 v$3)))" +
                 "(v$6 (cnt () (v$2 v$4)))" +
                 "(v$7 #t)" +
                 "(v$8 #f))" +
               "(if (!= v$7 v$8) v$5 v$6))))) (halt))")

  @Test def testTailNestedIf =
    testCPSHighTreeEquality("""
        (let ( (n (@< 2 3)) )
          (if (@= n #t)
            (@char-print 'O')
            (@char-print 'N')
          )
        )""", """
        (let* ((v$1
              (cnt (v$2)
                (let* ((v$3 (cnt (v$4) (halt)))
                       (v$5
                        (cnt ()
                          (let* ((v$6 'O') (v$7 (char-print v$6))) (v$3 v$7))))
                       (v$8
                        (cnt ()
                           (let* ((v$9 'N') (v$10 (char-print v$9))) (v$3 v$10))))
                       (v$11 #t))
                  (if (= v$2 v$11) v$5 v$8))))
             (v$12 (cnt () (let ((v$13 #t)) (v$1 v$13))))
             (v$14 (cnt () (let ((v$15 #f)) (v$1 v$15))))
             (v$16 2)
             (v$17 3))
        (if (< v$16 v$17) v$12 v$14))
        """)
}
