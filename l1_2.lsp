(defun a(V W)
	(COND 
		((null W) 0)
		(T(+ (* (CAR V) (CAR W)) (a (CDR V) (CDR W))))
	)
)

;nu merge
(defun mainb(L OK)
	(COND
		((null L) 0)
		((listp (CAR L)) (+ OK (mainb (CAR L) 1) (mainb (CDR L) 0)))
		(T (mainb (CDR L) OK))
	)
)

(defun b(L)
	(+ 1 (mainb L 1))
)

(defun contains(L A)
	(COND
		((null L) nil)
		((= (CAR L) A) T)
		(T (contains (CDR L) A))
	)
)

(defun c(V W)
	(COND
		((null V) nil)
		((contains W (CAR V)) (CONS (CAR V) (c (CDR V) W)))
		(T (c (CDR V) W))
	)
)

(defun testa()
	(assert (equal (a '(1 2 3) '(2 3 4)) 20))
	(assert (equal (a '(4 10 3) '(1 1 7)) 35))
	(assert (equal (a nil nil) 0))
)

(defun testa()
	(assert (equal (b '(1 2 (3 (4)))) 3))
	(assert (equal (b '(1 2 3 4)) 1))
	(assert (equal (b '(1 (3 (4)) (5 (6)) (7))) 3))
	(assert (equal (b nil) 1))
)

(defun testc()
	(assert (equal (c '(1 2 3) '(2 3 4)) '(2 3)))
	(assert (equal (c '(1 2 3 6 4) '(10 2 8 4 1)) '(1 2 4)))
	(assert (equal (c '(4 10 3) '(1 1 7)) nil))
	(assert (equal (c nil nil) nil))
)