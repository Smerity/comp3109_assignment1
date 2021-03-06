
(set 'operators '((s-eq . eql) (s-le . <) (s-ge . >) (s-leq . <=) (s-geq . >=) (s-diff . neq)))
; Clean function for converting between operator terms
(defun conv-string (str)
  (cdr (assoc str operators))
)

; Replaces the AND macro with a function so that it works with eval
(defun and2 (test1 test2)
  (and test1 test2)
)

; Replaces the OR macro with a function so that it works with eval
(defun or2 (test1 test2)
  (or test1 test2)
)

; Replaces the nilp predicate with a function so that it works well with eval
(defun nilp2 (x)
  (nilp x)
)

; Define a not equal function because Lisp does not recognise neq
(defun neq (x y)
  not (eql x y)
)

; Part 1:
;  This function has a series of conds which recursivly finds the union of all the columns in the query
(defun find-columns (pred)
	(cond 
	  ; Recurse for AND, OR and NOT
	  ( (eql 's-and (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) ))) 
	  ( (eql 's-or  (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) )))
	  ( (eql 's-not (first pred)) (find-columns (nth 1 pred)))
	  
	  ; The base case, when we are left with a single predicate return the column letter
	  ( T  (list (nth 1 pred)) )
	)
)

; Part 2:
; This is the section where the function is built.
; 
; The transformerR function is the recursive function that builds the main function.
(defun transformerR (query)
	(cond
		; Recurse for AND, OR and NOT cases.
		( (eq 's-and (first query) )
			; Use our and2 function along with the results from the 2 recursive calls
			`(and2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		( (eq 's-or (first query) ) 
			`(or2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		
		( (eq 's-not (first query)) 
			; recurses with the not's argument
			`(not ,(transformerR (nth 1 query) ) )
		)
		; The base case, recurse if its not in the AND OR or NOT case
		( T
			;If the row we are looking for does not exist, return null. This will make
			;AND not fail and make OR succeed when the 2nd argument has a non existent column
		   `(if (null (assoc ',(nth 1 query) row)) 
			     () 
				 ; Build the Lisp function that relates to the criteria we are processing
				 (,(conv-string (first query)) (nth 1 (assoc ',(nth 1 query) row)) ,(nth 2 query) )
			)
		)

	)	
)

;This function adds the lambda to the start of the function and then makes it a lambda closure
(defun transformer (query)
	;(format T "~a~%" query)
	(eval `(lambda(row) ,( transformerR query) ))
)
