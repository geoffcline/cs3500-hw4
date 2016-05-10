
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

(defun myCommon (L1 L2)
	(cond 
		((eq L1 nil) (nil))
		((eq myMember((car L1) L2) t) (list (car L1) (myCommon (cdr L1) L2)))
		((/= myMember((car L1) L2) t) (myCommon (cdr L1) L2))
	)
)

(defun myGen (X Y Z)
	(cond 
		((<= (+ X Y) Z) (list X (myGen (+ X Y) Y Z)))
		((> (+ X Y) Z) (nil))
	)
)

(defun myMap (F L)
	(cond
		((/= L nil) (list (F (car L)) (myMap F (cdr L))))
		((eq L nil) (nil))
	)
)

(defun myReduce (F L)
	(cond
		((/= (cdr (cdr L)) nil) (F (car L) (myReduce F (cdr L))))
		((eq (cdr (cdr L)) nil) (F (car L) (car (cdr L))))
 	)
)
