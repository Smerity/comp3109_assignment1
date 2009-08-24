;#!/usr/bin/gcl -f

(defun conv-string (str)
	(cond
	  ( (eql 's-eq str) 'eq)
	  ( (eql 's-le str) '<)
	  ( (eql 's-ge str) '>)
	  ( (eql 's-leq str) '<=)
	  ( (eql 's-geq str) '>=)
	  ( (eql 's-diff str) 'neq)
	)
)

(defun find-columns (pred)
	(cond 
	  ( (eql 's-and (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) ))) 
	  ( (eql 's-or  (first pred)) (union ( find-columns (nth 1 pred) ) ( find-columns (nth 2 pred) ))) 
	  ( (eql 's-not (first pred)) (find-colums rest(pred) ))
	  ( T  (list (nth 1 pred)) )
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

		( T 
			`(,(conv-string (first query)) (nth 1 (assoc ',(nth 1 query) row)) ,(nth 2 query) )
		)
	)	
)

(defun transformer (query)
	(format T "~a~%" query)
	`(lambda(row) ,( transformerR query) )
)

(setq tq (transformer '(s-and (s-geq B 5) (s-le B 100))))

(format T "~a~%"
	(list 
		'(funcall tq  ( (A 300) (B 20) ) ) 
		 (funcall tq '( (A 300) (B 20) ) )
	)
)




