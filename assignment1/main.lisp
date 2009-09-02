
(set 'operators '((s-eq . eq) (s-le . <) (s-ge . >) (s-leq . <=) (s-geq . >=) (s-diff . neq)))
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

; define a not equal function because lisp does not recognise neq
(defun neq (x y)
  ; Maybe I should be doing eql = "eq + =", where eq is true if same symbol
  (not (= x y))
)

; part 1:
;  this function has a series of conds which recursivly finds the union of all the columns in the query
(defun find-columns (pred)
	(cond 
	  ; Recurse for AND OR and NOT
	  ( (eql 's-and (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) ))) 
	  ( (eql 's-or  (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) )))
	  ( (eql 's-not (first pred)) (find-columns (rest pred) ))
	  
	  ; The base case, when we are left with a single predicate return the column letter
	  ( T  (list (first pred)) )
	)
)

; Part 2: this is the section where the function is built.
;
; The transformerR function is the recursive function that builds the main function.
(defun transformerR (query)
	(cond
		; Recurse for AND OR and NOT cases.
		( (eq 's-and (first query) )
			; use our and2 function along with the results from the 2 recursive calls
			`(and2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		
		( (eq 's-or (first query) ) 
			`(or2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		
		( (eq 's-not (first query)) 
			`(not ,(transformerR (rest query) ) )
		)
		; The base case, recurse if its not in the AND OR or NOT case
		( T
			;if the row we are looking for does not exist, return null this will make
			;AND not fail and make OR succeed when the 2nd argument has a non existant column
		   `(if (null (assoc ',(nth 1 query) row)) 
			     () 
				 ; build the lisp function that relates to the criteria we are processing
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

;(setq tq (transformer '(s-and (s-geq B 5) (s-le B 100))))

;(mapcar #tq spreadsheet)
;(mapcar #'(lambda (x) (and (#tq x) (list x))) spreadsheet)

;(format T "~a~%~%"
;	(list 
;		'(funcall tq  ( (A 300) (B 20) ) ) 
;		 (funcall tq '( (A 300) (B 20) ) )
;	)
;)

;(format T "Test - should be 'nil T nil'~%=-=-=-=-=~%")
;(setq q (transformer '(s-and (s-eq A 6)(s-ge B 100))))
;(format T "~a~%" 
;  (list 
;    (funcall q '( (A 7)(B 6)(C 9) ) )
;    (funcall q '( (A 6)(B 101) ) )
;    (funcall q '( (C 6) ) )
;    (funcall q '((Z 315) (B 324) (C 528) (D 618) (E 423) (F 275) (G 768) (H 689) (I 663) (J 266) (K 985) (L 94) (M 731) (N 903) (O 544) (P 702) (Q 558) (R 52) (S 34) (T 456) (U 43) (V 421) (W 97) (X 73) (Y 940) (A 756)) )
;  )
;)
