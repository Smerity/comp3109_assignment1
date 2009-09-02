(defun neq (x y)
  ; Maybe I should be doing eql = "eq + =", where eq is true if same symbol
  (not (= x y))
)

(set 'operators '((s-eq . eq) (s-le . <) (s-ge . >) (s-leq . <=) (s-geq . >=) (s-diff . neq)))
; Clean function for converting between operator terms
(defun conv-string (str)
  (cdr (assoc str operators))
)

(defun and2 (test1 test2)
  (and test1 test2)
)

(defun or2 (test1 test2)
  (or test1 test2)
)
(defun nilp2 (x)
  (nilp x)
)

(defun find-columns (pred)
	(cond 
	  ( (eql 's-and (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) ))) 
	  ( (eql 's-or  (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) )))
	  ; Spelling error on colum(n)s and rest(pred)
	  ( (eql 's-not (first pred)) (find-columns (rest pred) ))
	  ; Used nth 1 instead of nth 0 - but mayber first instead?
	  ( T  (list (first pred)) )
	)
)

(defun transformerR (query)
	;(format T "~a~%" query)
	(cond
		( (eq 's-and (first query) ) 
			`(and2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		
		( (eq 's-or (first query) ) 
			`(or2 ,(transformerR (nth 1 query)) ,(transformerR (nth 2 query)) ) 
		)
		
		( (eq 's-not (first query)) 
			`(not ,(transformerR (rest query) ) )
		)

    ; Issue with the way this base case goes ...
		( T 
			`(if (null (assoc ',(nth 1 query) row) ) () (,(conv-string (first query)) (nth 1 (assoc ',(nth 1 query) row)) ,(nth 2 query) ))
		)
	)	
)

(defun transformer (query)
	(format T "~a~%" query)
	(eval `(lambda(row) ,( transformerR query) ))
)

(setq tq (transformer '(s-and (s-geq B 5) (s-le B 100))))

;(mapcar #tq spreadsheet)
;(mapcar #'(lambda (x) (and (#tq x) (list x))) spreadsheet)

(format T "~a~%~%"
	(list 
		'(funcall tq  ( (A 300) (B 20) ) ) 
		 (funcall tq '( (A 300) (B 20) ) )
	)
)

(format T "Test - should be 'nil T nil'~%=-=-=-=-=~%")
(setq q (transformer '(s-and (s-eq A 6)(s-ge B 100))))
(format T "~a~%" 
  (list 
    (funcall q '( (A 7)(B 6)(C 9) ) )
    (funcall q '( (A 6)(B 101) ) )
    (funcall q '( (C 6) ) )
    (funcall q '((Z 315) (B 324) (C 528) (D 618) (E 423) (F 275) (G 768) (H 689) (I 663) (J 266) (K 985) (L 94) (M 731) (N 903) (O 544) (P 702) (Q 558) (R 52) (S 34) (T 456) (U 43) (V 421) (W 97) (X 73) (Y 940) (A 756)) )
  )
)
