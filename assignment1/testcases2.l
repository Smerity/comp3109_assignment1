; Test-cases for second task
;

(setq tq1 (transformer '(s-geq fuel 5))) 
(funcall tq1 '((speed 300) (fuel 20)))
; result T
(funcall tq1 '((speed 40) (fuel 100))) 
; result T

(setq tq2 (transformer '(s-and (s-geq fuel 5) (s-le fuel 100))))) 
(funcall tq2 '((speed 300) (fuel 20)))
; result T
(funcall tq2 '((speed 40) (fuel 100))) 
; result nil

(setq tq3 (transformer '(s-not (s-or (s-not (s-geq fuel 5)) (s-not (s-le fuel 100)))))) 
(funcall tq3 '((speed 300) (fuel 20)))
; result T
(funcall tq3 '((speed 40) (fuel 100)))
; result nil

; Association lists appear in two different formats (depending on LISP flavor, etc.). An 
; association list is either represented as ((key1.data1) (key2.data2) .. )  or 
; ((key1 data1) (key2 data2) ..). Both formats are allowed. If you use ((key1.data1) ...) 
; as a format, please rewrite the testcases accordingly. 
