(defun contains(L E)
	(COND
		((null L) nil)
		((= (CAR L) E) T)
		(T (contains (CDR L) E))
	)
)

(defun a(S1 S2)
	(COND
		((null S1) nil)
		((null (contains S2 (CAR S1))) (CONS (CAR S1) (a (CDR S1) S2)))
		(T (a (CDR S1) S2))
	)
)

(defun b(L)
	(COND
		((null L) nil)
		((listp (CAR L)) (CONS (reverse (b (CAR L))) (b (CDR L))))
		(T (CONS (CAR L) (b (CDR L))))
	)
)

(defun mainb(L)
	(reverse (b L))
)

(defun c(L)
	(COND
		((null L) nil)
		((listp (CAR L))
			(COND
				((= (mod (length (CAR L)) 2) 1) (APPEND (CONS (CAAR L) (c (CAR L))) (c (CDR L))))
				(T (APPEND (c (CAR L)) (c (CDR L))))
			)
		)
		(T (c (CDR L)))
	)
)

(defun mainc(L)
	(COND
		((= (mod (length L) 2) 1) (CONS (CAR L) (c L)))
		(T (c L))
	)
)

(defun d(L)
	(COND
		((null L) 0)
		((numberp (CAR L)) (+ (CAR L) (d (CDR L))))
		(T (d (CDR L)))
	)
)

(defun testa()
	(assert (equal (a '(1 2 3) '(1)) '(2 3)))
	(assert (equal (a '(1 2 3) '(1 2 3)) nil))
	(assert (equal (a '(10 4 8 2 5) '(27 3 20 2 4)) '(10 8 5)))
	(assert (equal (a '(10 4 8 9) '(4 8 9 10)) nil))
)

(defun testb()
	(assert (equal (mainb '(1 2 3)) '(3 2 1)))
	(assert (equal (mainb '(1 2 (7 3 1 (2)))) '(((2) 1 3 7) 2 1)))
	(assert (equal (mainb '(9 (2 3 (4 5)) (1 (2) 2) 0)) '(0 (2 (2) 1) ((5 4) 3 2) 9)))
	(assert (equal (mainb '(1 2 (3 4 (4 5 (6 7 (8)))) 0)) '(0 ((((8) 7 6) 5 4) 4 3) 2 1)))
)