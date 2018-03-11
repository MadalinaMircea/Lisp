(defun removeAll(L C)
	(COND
		((null L) nil)
		((equal (CAR L) C) (removeAll (CDR L) C))
		(T (CONS (CAR L) (removeAll (CDR L) C)))
	)
)

(defun frequency(L A)
	(COND
		((null L) 0)
		((equal A (CAR L)) (+ 1 (frequency (CDR L) A)))
		(T (frequency (CDR L) A))
	)
)

(defun d(L)
	(COND
		((null L) nil)
		(T (CONS (CONS (CAR L) (list (frequency L (CAR L)))) (d (removeAll (CDR L) (CAR L)))))
	)
)