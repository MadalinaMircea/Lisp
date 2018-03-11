(defun removeOne(L E)
	(cond
		((null L) nil)
		((equal (car L) E) (cdr L))
		(T (cons (car L) (removeOne (cdr L) E)))
	)
)

(defun a(s1 s2)
	(cond
		((null s1) s2)
		(T (cons (car s1) (a (cdr s1) (removeOne s2 (car s1)))))
	)
)

(defun b(L)
	(COND
		((null L) 1)
		((listp (CAR L)) (* (b (CAR L)) (b (CDR L))))
		(T (* (CAR L) (b (CDR L))))
	)
)

(defun minimum(L)
	(COND
		((null L) nil)
		((null (CDR L))
			(COND
				((listp (CAR L)) (minimum (CAR L)))
				(T (CAR L))
			)
		)
		((listp (CAR L))
			(COND
				((< (minimum (CAR L)) (minimum (CDR L))) (minimum (CAR L)))
				(T (minimum (CDR L)))
			)
		)
		(T (min (CAR L) (minimum (CDR L))))
	)
)

(defun c(L)
	(COND
		((null L) nil)
		(T (CONS (minimum L) (c (removeOne L (minimum L)))))
	)
)

(defun d(L P)
	(COND
		((null L) nil)
		((equal (CAR L) (minimum L)) (CONS P (d (CDR L) (+ P 1))))
		(T (d (CDR L) (+ P 1)))
	)
)

(defun maind(L)
	(d L 1)
)