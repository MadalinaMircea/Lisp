(defun a(L N)
	(COND
		((null L) nil)
		((= N 1) (CDR L))
		(T (CONS (CAR L) (a (CDR L) (- N 1))))
	)
)

(defun b(L C)
	(COND
		((null L)
			(COND
				((= C 0) nil)
				(T (list C))
			)
		)
		((= (+ (CAR L) C) 10) (CONS 0 (b (CDR L) 1)))
		(T (CONS (+ (CAR L) C) (b (CDR L) 0)))
	)
)

(defun mainb(L)
	(reverse (b (reverse L) 1))
)

(defun removeAll(L C)
	(COND
		((null L) nil)
		((= (CAR L) C) (removeAll (CDR L) C))
		(T (CONS (CAR L) (removeAll (CDR L) C)))
	)
)

(defun toLinear(L)
	(COND
		((null L) nil)
		((listp (CAR L)) (APPEND (toLinear (CAR L)) (toLinear (CDR L))))
		(T (CONS (CAR L) (toLinear (CDR L))))
	)
)

(defun c(L)
	(COND
		((null L) nil)
		(T (CONS (CAR L) (c (removeAll L (CAR L)))))
	)
)

(defun mainC(L)
	(c (toLinear L))
)

(defun contains(L E)
	(COND
		((null L) nil)
		((= (CAR L) E) T)
		(T (contains (CDR L) E))
	)
)

(defun d(L)
	(COND
		((null L) T)
		((contains (CDR L) (CAR L)) nil)
		(T (d (CDR L)))
	)
)


(defun testb()
	(assert (equal (mainb '(1 2 3)) '(1 2 4)))
	(assert (equal (mainb '(6 2 0 9)) '(6 2 1 0)))
	(assert (equal (mainb '(9 1 9 9)) '(9 2 0 0)))
	(assert (equal (mainb '(9 9)) '(1 0 0)))
)

(defun testa()
	(assert (equal (a '(1 2 3) 2) '(1 3)))
	(assert (equal (a '(1 10 4 9 25 3) 4) '(1 10 4 25 3)))
	(assert (equal (a '(1 2 3) 5) '(1 2 3)))
	(assert (equal (a nil 2) nil))
)

(defun testc()
	(assert (equal (mainc '(1 2 3)) '(1 2 3)))
	(assert (equal (mainc '(1 9 (2 7 (1 2 (9))))) '(1 9 2 7)))
	(assert (equal (mainc '(1 (2 (1 3 (2 4) 3) 1) (1 4)) ) '(1 2 3 4)))
)

(defun testd()
	(assert (equal (d '(1 2 3)) T))
	(assert (equal (d '(1 9 2 7 9 2)) nil))
	(assert (equal (d '(1 1 9 2 7 8) ) nil))
	(assert (equal (d '(1 2 3 7 4 10 19 5)) T))
)