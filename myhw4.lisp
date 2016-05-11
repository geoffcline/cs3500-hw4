
(defun myLast (L)
	(cond
		((eq (cdr L) nil) (car L))
		((not (eq (cdr L) nil)) (myLast (cdr L)))
	)
)

(defun myCount (X L)
	(cond 
		((eq L nil) 0)
		((eq (car L) X) (+ 1 (myCount X (cdr L))))
		((not (eq (car L) X)) (myCount X (cdr L)) )
	)
)

(defun myMember (X L)
	(> (myCount X L) 0)
)

(defun myRemove (X L)
	(cond
		((eq L nil) nil)
		((eq (car L) X) (myRemove X (cdr L)))
		((not (eq (car L) X)) (cons (car L) (myRemove X (cdr L))))
	)
)

(defun myPurge (L)
	(cond 
		((eq L nil) nil)
		((not (eq L nil)) (cons (car L) (myPurge (myRemove (car L) (cdr L)))))
	)
)

(defun myCommon (L1 L2)
	(cond 
		((eq L1 nil) (nil))
		((eq (myMember (car L1) L2) t) (cons (car L1) (myCommon (cdr L1) L2) ) )
		((not (eq (myMember (car L1) L2) t)) (myCommon (cdr L1) L2))
	)
)

(defun myGen (X Y Z)
	(cond 
		((<= (+ X Y) Z) (cons X (myGen (+ X Y) Y Z)))
		((> (+ X Y) Z) (nil))
	)
)

(defun myMap (F L)
	(cond
		((not (eq L nil)) (cons (F (car L)) (myMap F (cdr L))))
		((eq L nil) (nil))
	)
)

(defun myReduce (F L)
	(cond
		((not (eq (cdr (cdr L)) nil)) (F (car L) (myReduce F (cdr L))))
		((eq (cdr (cdr L)) nil) (F (car L) (car (cdr L))))
 	)
)
