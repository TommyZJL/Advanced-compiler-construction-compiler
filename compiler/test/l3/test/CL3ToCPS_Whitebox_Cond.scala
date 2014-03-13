package l3.test

import l3.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CL3ToCPS_Whitebox_Cond extends CPSHighTest {

  @Test def testCondNestedTrueTrue =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #t #t) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 3)" +
             "(v$8 4))" +
          "(if (= v$7 v$8) v$3 v$3))")

  @Test def testCondNestedFalseFalse =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #f #f) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 3)" +
             "(v$8 4))" +
          "(if (= v$7 v$8) v$5 v$5))")

  @Test def testCondNestedTrueFalse =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #t #f) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 3)" +
             "(v$8 4))" +
          "(if (= v$7 v$8) v$3 v$5))")

  @Test def testCondNestedFalseTrue =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #f #t) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 3)" +
             "(v$8 4))" +
          "(if (= v$7 v$8) v$5 v$3))")

  @Test def testCondNestedTrueExpr =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #t (if (@ = 3 4) #t #f)) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 (cnt () (let* ((v$8 3) (v$9 4)) (if (= v$8 v$9) v$3 v$5))))" +
             "(v$10 3)" +
             "(v$11 4))" +
          "(if (= v$10 v$11) v$3 v$7))")

  @Test def testCondNestedFalseExpr =
    testCPSHighTreeEquality("(if (if (@ = 3 4) #f (if (@ = 3 4) #t #f)) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 (cnt () (let* ((v$8 3) (v$9 4)) (if (= v$8 v$9) v$3 v$5))))" +
             "(v$10 3)" +
             "(v$11 4))" +
          "(if (= v$10 v$11) v$5 v$7))")

  @Test def testCondNestedExprFalse =
    testCPSHighTreeEquality("(if (if (@ = 3 4) (if (@ = 3 4) #t #f) #f) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 (cnt () (let* ((v$8 3) (v$9 4)) (if (= v$8 v$9) v$3 v$5))))" +
             "(v$10 3)" +
             "(v$11 4))" +
          "(if (= v$10 v$11) v$7 v$5))")

  @Test def testCondNestedExprTrue =
    testCPSHighTreeEquality("(if (if (@ = 3 4) (if (@ = 3 4) #t #f) #t) 1 2)",
      "(let* ((v$1 (cnt (v$2) (halt)))" +
             "(v$3 (cnt () (let ((v$4 1)) (v$1 v$4))))" +
             "(v$5 (cnt () (let ((v$6 2)) (v$1 v$6))))" +
             "(v$7 (cnt () (let* ((v$8 3) (v$9 4)) (if (= v$8 v$9) v$3 v$5))))" +
             "(v$10 3)" +
             "(v$11 4))" +
          "(if (= v$10 v$11) v$7 v$3))")
}
