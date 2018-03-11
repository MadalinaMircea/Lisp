(defun contains(L E)
	(COND
		((null L) nil)
		((equal (CAR L) E) T)
		(T (contains (CDR L) E))
	)
)

(defun d(S1 S2)
	(COND
		((null S1) T)
		((not (contains S2 (CAR S1))) nil)
		(T (d (CDR S1) S2))
	)
)

(defun maind(S1 S2)
	(COND
		((not (equal (length S1) (length S2))) nil)
		(T (d S1 S2))
	)
)