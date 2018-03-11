(defun parse(L)
	(COND 
		((null L) nil)
		((equal (CAR (CDR L)) 0) (CONS (CAR L) (parse (CDDR L))))
		((equal (CAR (CDR L)) 1) (APPEND (parse (CDDR L)) (list (CAR L))))
		(T (APPEND (APPEND (parse (CDDR L)) (parse (CDDR (CDDR L)))) (list (CAR L))))
	)
)

(parse '(A 2 B 0 C 2 D 0 E 0))
(parse '(1 2 2 2 4 0 5 0 3 0))