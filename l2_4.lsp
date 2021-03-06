(defun 2to1(L)
	(COND ((null L) nil)
		  ((atom (CAR L)) (APPEND (APPEND (list (CAR L)) (list (- (length L) 1))) (2to1 (CDR L))))
		  ((listp (CAR L)) (APPEND (2to1 (CAR L)) (2to1 (CDR L))))
	)
)

(defun test2to1()
	(assert (equal (2to1 '(A (B) (C (D) (E)))) '(A 2 B 0 C 2 D 0 E 0)))
	(assert (equal (2to1 '(2 (3 (2) (1)) (6 (4 (3))))) '(2 2 3 2 2 0 1 0 6 1 4 1 3 0)))
	(assert (equal (2to1 '(a (b (c (d (e)))))) '(a 1 b 1 c 1 d 1 e 0)))
	(assert (equal (2to1 nil) nil))
)