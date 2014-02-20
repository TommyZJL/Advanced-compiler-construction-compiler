package l3.test.ok
import org.junit.Test

trait DesugaringOKTests {
  this: AllOKTests =>

  @Test def testDesugaringBegin =
    compileAndInterpret("""
      (begin
        (@char-print 'O')
        (@char-print 'K')
        (@char-print (@int->char 10)))
    """)

  @Test def testDesugaringCond =
    compileAndInterpret("""
      (cond ((@= 1 2) (@char-print '*'))
            ((@= 1 1) (@char-print 'O'))
            ((@= 1 1) (@char-print '*')))

      (cond ((@= 1 1) (@char-print 'K'))
            ((@= 1 2) (@char-print '*'))
            ((@= 1 2) (@char-print '*')))

      (cond ((@< 1 1) (@char-print '*'))
            ((@< 1 0) (@char-print '*'))
            ((@= 1 1) (@char-print (@int->char 10))))
    """)

  @Test def testDesugaringIf =
    compileAndInterpret("""
      (@char-print (if (@unit? (if #f 1)) 'O' 'K'))
      (@char-print (if (@unit? (if #t 1)) 'O' 'K'))
      (@char-print (@int->char 10))
    """)

  @Test def testDesugaringLets =
    compileAndInterpret("""
      (let* ((K1 'K')
             (O1 'O')
             (O O1)
             (K K1))
        (@char-print O)
        (@char-print K)
        (@char-print (@int->char 10)))
    """)

  @Test def testDesugaringRec =
    compileAndInterpret("""
      (def ok-str
           (fun (i)
                (cond ((@= i 0) 'O')
                      ((@= i 1) 'K')
                      ((@= i 2) (@int->char 10)))))

      (rec loop ((i 0))
           (if (@<= i 2)
               (begin
                 (@char-print (ok-str i))
                 (loop (@+ i 1)))))
    """)

  @Test def testDesugaringStrings =
    compileAndInterpret("""
      (def ok-str "OK")
      (@char-print (@block-get ok-str 0))
      (@char-print (@block-get ok-str 1))
      (@char-print (@int->char 10))
    """)
}
