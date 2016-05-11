;Geoffrey Cline
;CS3500, A, HW4
;May 6th, 2016

;returns the last atom of a list
(defun myLast (L)
	(cond
		((eq (cdr L) nil) (car L))
		((not (eq (cdr L) nil)) (myLast (cdr L)))
	)
)

;returns the number of times an atom appears in a list
(defun myCount (X L)
	(cond 
		((eq L nil) 0)
		((eq (car L) X) (+ 1 (myCount X (cdr L))))
		((not (eq (car L) X)) (myCount X (cdr L)) )
	)
)

;retuns if true if an atom is a list
(defun myMember (X L)
	(> (myCount X L) 0)
)

;removes all appearances of atom x from list l
(defun myRemove (X L)
	(cond
		((eq L nil) nil)
		((eq (car L) X) (myRemove X (cdr L)))
		((not (eq (car L) X)) (cons (car L) (myRemove X (cdr L))))
	)
)

;the count of all atoms in the resulting list will be at most 1
;the first appearance is kept
(defun myPurge (L)
	(cond 
		((eq L nil) nil)
		((not (eq L nil)) (cons (car L) (myPurge (myRemove (car L) (cdr L)))))
	)
)

;returns the elements in both L1 and L1 in the order of L1
(defun myCommon (L1 L2)
	(cond 
		((eq L1 nil) nil)
		((eq (myMember (car L1) L2) t) (cons (car L1) (myCommon (cdr L1) L2) ) )
		((not (eq (myMember (car L1) L2) t)) (myCommon (cdr L1) L2))
	)
)

;returns a list of atoms starting at X, incrementing by Y, up to and including Z
(defun myGen (X Y Z)
	(cond 
		((<  X Y) (cons X (myGen (+ X Z) Y Z)))
		((eq X Y) (cons X (myGen (+ X Z) Y Z)))
		((>  X Y) nil)
	)
)

;applies function F to each item in list L
(defun myMap (F L)
	(cond
		((not (eq L nil)) (cons (funcall F (car L)) (myMap F (cdr L))))
		((eq L nil) nil)
	)
)

;
(defun myReduce (F L)
	(cond
		((eq (cdr L) nil) (car L))
		((not (eq (cdr L) nil)) (funcall F (car L) (myReduce F (cdr L))))
 	)
)