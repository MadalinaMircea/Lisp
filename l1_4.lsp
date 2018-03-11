(defun vSum(L1 L2)
(COND   ((AND (null L1) (null L2)) nil)
	((AND (null L1) (not (null L2))) (CONS (+ 0 (CAR L2)) (vSum L1 (CDR L2))))
	((AND (not (null L1)) (null L2)) (CONS (+ (CAR L1) 0) (vSum (CDR L1) L2)))
	(T (CONS (+ (CAR L1) (CAR L2)) (vSum (CDR L1) (CDR L2))))
)
)

(defun vectorSum(L1 L2)
	(reverse (vSum (reverse L1) (reverse L2)))
)

(defun getAtoms(L)
	(COND ((null L) nil)
		  ((listp (CAR L)) (APPEND (getAtoms (CAR L)) (getAtoms (CDR L))))
		  (T (CONS (CAR L) (getAtoms (CDR L))))
	)
)

(defun reverseAll(L aux)
	(COND ((null L) (reverse aux))
		  ((listp (CAR L)) (APPEND (reverse aux) (CONS (reverseAll (CAR L) nil) (reverseAll (CDR L) nil))))
		  (T (reverseAll (CDR L) (APPEND aux (list (CAR L)))))
	)
)

(defun reverseMain(L)
	(reverseAll L nil)
)

(defun maxValue(L)
	(COND ((null L) -100000)
		  ((listp (CAR L)) (maxValue (CDR L)))
		  (T (max (maxValue (CDR L)) (CAR L)))
	)
)

(defun testa()
	(assert (equal (vSum '(1 2 3) '(1)) '(2 2 3)))
	(assert (equal (vSum '(1 6 4 2) '(8 2 5 3)) '(9 8 9 5)))
	(assert (equal (vSum '(1 9 5 3) '(5 2 1)) '(6 11 6 3)))
	(assert (equal (vSum nil nil) nil))
)

(defun testb()
	(assert (equal (getAtoms '(1 2 (7 3 2) 3 (2 3) 8 9 (1))) '(1 2 7 3 2 3 2 3 8 9 1)))
	(assert (equal (getAtoms '(10 4 2)) '(10 4 2)))
	(assert (equal (getAtoms '((3 4) 10 (22 3 1 (3)))) '(3 4 10 22 3 1 3)))
	(assert (equal (getAtoms '()) '()))
)

(defun testc()
	(assert (equal (reverseMain '(1 2 (4 7 9 (7 6) 3))) '(2 1 (9 7 4 (6 7) 3))))
	(assert (equal (reverseMain '(a b c (d (e f) g h i))) ' (c b a (d (f e) i h g))))
	(assert (equal (reverseMain nil) nil))
	(assert (equal (reverseMain '(1)) '(1)))
)

(defun testd()
	(assert (= (maxValue '(1 4 10 2)) 10))
	(assert (= (maxValue '(29 45 (55 2 8) 10 2 33)) 45))
	(assert (= (maxValue '(0)) 0))
	(assert (= (maxValue nil) -100000))
)

(defun testAll()
	(testa)
	(testb)
	(testc)
	(testd)
)