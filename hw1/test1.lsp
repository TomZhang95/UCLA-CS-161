(load "hw1.lsp")

;1
(print (IS-SAME (TREE-CONTAINS 7 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) NIL))

;2 3
(print (equal (TREE-CONTAINS 13 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) T))


;4 5 6
(print (equal (TREE-MAX '((1 2 3) 7 8)) 8))
(print (equal (TREE-MAX '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) 13))


;7 8 9 10
(print (IS-SAME (TREE-ORDER '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) '(1 2 3 5 6 8 9 10 11 12 13)))
(print (IS-SAME (TREE-ORDER 4) '(4)))

(print "passed 10")

;11 12 13 14
(print (IS-SAME (SUB-LIST '(1 2 3) 0 1) '(1)))
(print (IS-SAME (SUB-LIST '(1 2 3) 0 0) NIL))
(print (IS-SAME (SUB-LIST '(1 2 3) 1 0) NIL))
;(print (IS-SAME (SUB-LIST '(1 2 4 7) 3 2) '(7)))

(print "passed 14")

;15 16 17 18

; (print (IS-SAME (SPLIT-LIST '(1)) '(NIL (1))))
(print (IS-SAME (SPLIT-LIST '(1 2)) '((1) (2))))
(print (IS-SAME (SPLIT-LIST '(1 2 3 4 5)) '((1 2) (3 4 5))))

(print "passed 18")

;19 20 21 22 23
(print (equal (BTREE-HEIGHT 1) 0))
(print(equal (BTREE-HEIGHT '(1 2)) 1))
(print (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3))
(print (equal (BTREE-HEIGHT '(1 ((2 3) 3))) 3))

(print "passed 23")

;24 25 26
(print(equal (LIST2BTREE '(1)) 1))
(print (IS-SAME (LIST2BTREE '(1 2 3 4 5)) '((1 2) (3 (4 5)))))
(print (IS-SAME (LIST2BTREE '(1 2 3 4 5 6 7)) '((1 (2 3)) ((4 5) (6 7)))))


(print "passed 26")

;27 28 29 30
(print (IS-SAME (BTREE2LIST 1) '(1)))
(print (IS-SAME (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))
(print (IS-SAME (BTREE2LIST (LIST2BTREE '(1))) '(1)))
(print (IS-SAME (BTREE2LIST (LIST2BTREE '(1 2 3 4 5))) '(1 2 3 4 5)))

(print "passed 30")

;31 32 33 34
(print (equal (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T))
(print (equal (IS-SAME '((1 2 3) 7 9) '((1 2 3) 7 8)) NIL))

(print (equal (IS-SAME '() '()) T))

(print "passed 34")
;35 36 37 38

(print (IS-SAME (IS-SAME '(1 (2 3 4)) '(1 2 3 4)) Nil))
(print (IS-SAME (IS-SAME '(1 (2 3 4)) '((1 2 3) 4)) Nil))
(print (equal (IS-SAME '(1 (2 3 4)) '(1 (2 3 4))) T))

;39 40
(print (equal (TREE-CONTAINS 3 '((1 2 3) 7 8)) T))
(print (equal (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL))

(print "passed 40")

;41 42 43
(print(equal (TREE-MAX '((1 2 3) 7 8)) 8))
(print (IS-SAME (TREE-ORDER 3) '(3)))
(print (IS-SAME (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8)))

;44 45 46
(print (SUB-LIST '(a b c d) 0 3)) ;(a b c)))
(print (SUB-LIST '(a b c d) 3 1)) ;(d)))
(print (SUB-LIST '(a b c d) 2 0)) ;NIL))


(print "passed 46")

;47 48 49
(print  (SPLIT-LIST '(a b c d e))) ;((a b)(c d e))))
(print  (SPLIT-LIST '(a b c d ))) ;((a b)(c d ))))
(print  (SPLIT-LIST '(a b c d e f))) ;((a b c)(d e f))))
(print  (SPLIT-LIST '(a b c d e f g)))

;(print(equal( (BTREE-HEIGHT 1) 0 )))
(print (BTREE-HEIGHT '(1 2)) );  1)))
(print (BTREE-HEIGHT '(1 (2 3))));  2)))
(print (BTREE-HEIGHT '((1 2)(3 4)))) ; 2)))
(print (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))) ;3)))


(print (SUB-LIST '(a b c d) 3 2)) ;NIL))
(print (equal (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T))
(print (equal (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL))

(print (IS-SAME (BTREE2LIST (LIST2BTREE '(1 2 3 4 5 6))) '(1 2 3 4 5 6)))
(print (IS-SAME (BTREE2LIST (LIST2BTREE '(1 2 3 4 5 6 7))) '(1 2 3 4 5 6 7)))


(Print "passed all tests")