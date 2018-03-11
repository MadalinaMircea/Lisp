(defun a(L1 L2)
	(COND
		((AND (null L1) (null L2)) nil)
		((AND (null L1) (not (null L2))) L2)
		((AND (null L2) (not (null L1))) L1)
		((< (CAR L1) (CAR L2)) (CONS (CAR L1) (a (CDR L1) L2)))
		(T (CONS (CAR L2) (a L1 (CDR L2))))
	)
)

(defun b(L E L1)
	(COND
		((null L) nil)
		((equal E (CAR L)) (APPEND L1 (b (CDR L) E L1)))
		((listp (CAR L)) (CONS (b (CAR L) E L1) (b (CDR L) E L1)))
		(T (CONS (CAR L) (b (CDR L) E L1)))
	)
)

(defun c(L1 L2 Power Carry)
	(COND
		((AND (null L1) (null L2)) 0)
		((null L1) (+ (* (+ (CAR L2) Carry) (expt 10 Power)) (c L1 (CDR L2) (+ Power 1) 0)))
		((null L2) (+ (* (+ (CAR L1) Carry) (expt 10 Power)) (c (CDR L1) L2 (+ Power 1) 0)))
		((> (+ (CAR L1) (CAR L2) Carry) 10)
			(+ (* (mod (+ (CAR L1) (CAR L2) Carry) 10) (expt 10 Power)) (c (CDR L1) (CDR L2) (+ Power 1) (floor (+ (CAR L1) (CAR L2) Carry) 10)))
		)
		(T (+ (* (+ (CAR L1) (CAR L2) Carry) (expt 10 Power)) (c (CDR L1) (CDR L2) (+ Power 1) 0)))
	)
)

(defun mainc(L1 L2)
	(c (reverse L1) (reverse L2) 0 0)
)

(defun testa()
	(assert (equal (a '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))
	(assert (equal (a '(1 2 3) '(2 3 4)) '(1 2 2 3 3 4)))
)

(defun testb()
	(assert (equal (b '(1 2 3) 2 '(5 6)) '(1 5 6 3)))
	(assert (equal (b '(1 (8 3 (2 1 1 (1)) 9 1)) 1 '(2 2 2)) '(2 2 2 (8 3 (2 2 2 2 2 2 2 (2 2 2)) 9 2 2 2))))
)