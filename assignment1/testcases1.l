; Test-cases for first task
;

(find-columns '(s-eq a1 1))
; Result should be (a1)

(find-columns '(s-le a2 2))
; Result should be (a2)

(find-columns '(s-ge a3 3))
; Result should be (a3)

(find-columns '(s-leq a4 4))
; Result should be (a4)

(find-columns '(s-geq a5 5))
; Result should be (a5)

(find-columns '(s-diff a6 6))
; Result should be (a6)

(find-columns '(s-not (s-eq a7 1)))
; Result should be (a7)

(find-columns '(s-or (s-eq a8 1) (s-eq a9 1)))
; Result should be (a8 a9)

(find-columns '(s-or (s-eq a10 1) (s-eq a11 21)))
; Result should be (a10 a11)

(find-columns '(s-and (s-eq a12 1) (s-eq a13 1)))
; Result should be (a12 a13)

(find-columns '(s-or (s-geq fuel 5) (s-le fuel 100)))
; Result should be (fuel)

