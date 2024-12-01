#lang racket


; • CSC324 — 2023W — Exercise 3

; Due: Mon Feb 20th 5PM..

; Higher-order functions


; Implement X to produce the cartesian product of two lists.
; Write your answer here. 
; Half of the marks are for using higher-order functions
; to minimize the use of recursion. You may write helper functions.


(provide (contract-out 
                       (X (list? list? . -> . list?))))

(require rackunit)
(module+ test (require rackunit))

; · cartesian-products


(module+ test
  (check-equal?  (X '(a b c) '(d e f)) '((a d) (a e) (a f)
(b d) (b e) (b f)
(c d) (c e) (c f))))




(define (X list1 list2)
  (Delist (map (λ (x) (map (λ (y) (list x y)) list1) ) list2) ))


  
  ;(map (lambda (x) (map (lambda (y) (list x y)) list2)) list1) )
  
  ;(map Mapper (list list1) (list list2)))


#|
(define (Delist lst1 lst2)
  (append lst1 lst2))
(Delist '(1 2 3) '(4 5 6))
|#

(define (Delist lst)
  (map (λ (y) (map (λ (x) (append x '()) y)) lst) ))


(define (Helper ele lst)
  ;(map (lambda (l) (append '() ele l)) lst))
  (map (lambda (x) (list x)) lst))

;(Helper (3 '(7 8 9)))




#;
(define (Mapper list1 ele2)
  (map (λ (x) (append (list ele2) x)) list1))

#;
(Mapper '(1 2 3) 9)
#;
(Mapper '(1 2 3) 8)

#;
(append (map (λ (x) (Mapper '(a b c) x)) '((9)(8) (7))))
 
;(append (Mapper '(a b c) 9) (Mapper '(a b c) 8) (Mapper '(a b c) 7))



  ;(map (λ (l1 l2) (append l1 l2)) (map list list1) (map list list2)))


