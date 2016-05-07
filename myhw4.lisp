(defun triple (X)
	(* 3 X)
)

(defun myLast (L)
	(car (myRev L))
)

(defun myRev (L)
	(list (myRev (cdr L)) (car L)) 
)

(defun myCount (X L)
	(cond 
		((eq L nil) 0)
		((eq (car L) X) (+ 1 myCount(X (cdr L))))
		((/= (car L) X) (myCount (X (cdr L))))
	)
)

(defun myMember (X L)
	(> (myCount X L) 0)
)

(defun myRemove (X L)
	(cond
		((eq L nil) nil)
		((eq (car L) X) (myRemove X (cdr L)))
		((/= (car L) X) (list (car L) (myRemove X (cdr L))))
	)
)

(defun myPurge (L)
	(list (car L) (myPurge (myRemove X (cdr L))))
)