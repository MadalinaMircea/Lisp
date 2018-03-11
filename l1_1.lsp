(defun a(L N)
	(COND 
		((null L) nil)
		((equal N 1) (CAR L))
		(T (a (CDR L) (- N 1)))
	)
)

(defun b(L A)
	(COND
		((null L) nil)
		((equal (CAR L) A) T)
		((listp (CAR L)) (OR (b (CAR L) A) (b (CDR L) A)))
		(T (b (CDR L) A))
	)
)

(defun c(L)
	(COND
		((null L) nil)
		((numberp (CAR L)) (c (CDR L)))
		((listp (CAR L)) (CONS (CAR L) (CONS (c (CAR L)) (c (CDR L)))))
	)
)

(defun mainC(L)
	(CONS L (c L))
)

(defun removeAll(L A)
	(COND
		((null L) nil)
		((equal (CAR L) A) (removeAll (CDR L) A))
		(T (CONS (CAR L) (removeAll (CDR L) A)))
	)
)

(defun d(L)
	(COND
		((null L) nil)
		(T (CONS (CAR L) (d (removeAll (CDR L) (CAR L)))))
	)
)

(defun testa()
	(assert (equal (a '(1 2 3) 2) 2))
	(assert (equal (a '(1 2 3) 0) nil))
	(assert (equal (a '(1 8 5 3) 10) nil))
)

(defun testb()
	(assert (equal (b '(1 2 3) 2) T))
	(assert (equal (b '(1 8 (4 5) 2) 4) T))
	(assert (equal (b '(9 10 4 (2 5) 3) 7) nil))
)

(defun testc()
	(assert (equal (mainC '(1 2 (4 5) (5 (2)))) '((1 2 (4 5) (5 (2))) (4 5) (5 (2)) (2))))
	(assert (equal (mainC '(1 2 (3 (4 5) (6 7)) 8 (9 10))) '( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) )))
)

(defun testd()
	(assert (equal (d '(1 8 4 2 1 4 1 3)) '(1 8 4 2 3)))
	(assert (equal (d '( 1 1 1 3 3 3 9 0 1 2)) '(1 3 9 0 2)))
)