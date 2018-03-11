(defun a(L N)
	(COND
		((null L) nil)
		((= N 1) (CONS (CAR L) (CONS (CAR L) (a (CDR L) (- N 1)))))
		(T (CONS (CAR L) (a (CDR L) (- N 1))))
	)
)

(defun b(V W)
	(COND
		((null V) nil)
		(T (CONS (CONS (CAR V) (CAR W)) (b (CDR V) (CDR W))))
	)
)

(defun d(L)
	(COND
		((null L) 0)
		((numberp (CAR L)) (+ 1 (d (CDR L))))
		(T (d (CDR L)))
	)
)

(defun testa()
	(assert (equal (a '(1 2 3) 2) '(1 2 2 3)))
	(assert (equal (a '(10 20 30 40 50) 3) '(10 20 30 30 40 50)))
	(assert (equal (a '(7 2 10 9) 1) '(7 7 2 10 9)))
	(assert (equal (a '(7 2 10 9) 4) '(7 2 10 9 9)))
	(assert (equal (a '(7 2 10 9) 5) '(7 2 10 9)))
	(assert (equal (a nil 1) nil))
)

(defun testb()
	(assert (equal (b '(1 2) '(5 6)) '((1 . 5) (2 . 6))))
	(assert (equal (b '(A B C) '(X Y Z)) '((A . X) (B . Y) (C . Z))))
	(assert (equal (a nil nil) nil))
)

(defun testd()
	(assert (equal (d '(1 2 a 5 6)) 4))
	(assert (equal (d '(1 a b c 3 k)) 2))
	(assert (equal (d '(a b c k)) 0))
	(assert (equal (d nil) 0))
)