(defun a(L A P)
	(COND
		((AND (null L) (= (mod P 2) 0)) nil)
		((AND (null L) (= P 1)) nil)
		((AND (AND (null L) (= (mod P 2) 1)) (> P 1)) (list A))
		((AND (> P 1) (= (mod P 2) 1)) (CONS A (CONS (CAR L) (a (CDR L) A (+ P 1)))))
		(T (CONS (CAR L) (a (CDR L) A (+ P 1))))
	)
)

(defun maina(L A)
	(a L A 1)
)

(defun d(L A)
	(COND
		((null L) 0)
		((listp (CAR L)) (+ (d (CAR L) A) (d (CDR L) A)))
		((AND (atom (CAR L)) (equal (CAR L) A)) (+ 1 (d (CDR L) A)))
		(T (d (CDR L) A))
	)
)

(defun testd()
	(assert (equal (d '(1 2 3) 1) 1))
	(assert (equal (d '(1 2 3 2 9 0 2) 2) 3))
	(assert (equal (d '(1 (2 1) (3 1 ( 3 3 1 (1)))) 1) 5))
	(assert (equal (d nil 1) 0))
)

(defun testa()
	(assert (equal (maina '(1 2 3) 1) '(1 2 1 3)))
	(assert (equal (maina '(1 2 3 2 9 0 2) 2) '(1 2 2 3 2 2 9 0 2 2)))
	(assert (equal (maina '(1 9 2 0 3 10 3 2) 1) '(1 9 1 2 0 1 3 10 1 3 2 1)))
	(assert (equal (maina '() 1) nil))
)