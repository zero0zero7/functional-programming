#lang racket ; CSC324 — 2023W — Assignment 1 — Eva Implementation

; Task: implement eva according to A1.eva-test.rkt.

(provide (contract-out (eva (any/c . -> . any/c)))
         ; Add any helper functions you tested in A1.eva-test.rkt.
         ; Whether you add contracts to them is optional.
         #;a-helper
         eva-func
         eva-func-call)

; Produce the value of a closed term from LCA.

#;(define (eva term) term)


(define (eva term)
  (match term    
    [(list func arg) (eva-func-call (eva-func func) (eva arg))]
    [literal literal]
  )
)

(define (eva-func func)
  (match func
    [(list 'λ (list param) body) (list param body)]
    )
  )


(define (eva-func-call lst-param-body arg)
  (if (list? (second lst-param-body))
      ; body is list --> if body-ele == param, map it to argument ; else body-ele
      (eva (map (λ (body-ele) (if (equal? body-ele (first lst-param-body))
                         (eva arg)
                         body-ele))
           (second lst-param-body)))
      ; body is a single element (not list)
      (cond
        [(equal? (first lst-param-body) (second lst-param-body)) (eva arg)] ;body is just 1 param
        [else (second lst-param-body)] ;body is literal
        )
      )
  )

#;(define (eva-func-call lst-param-body arg)
  (if (list? (second lst-param-body))
      ; body is list --> if body-ele == param, map it to argument ; else body-ele
      (map (λ (body-ele) (if (equal? body-ele (first lst-param-body))
                         arg
                         body-ele))
           (second lst-param-body))
      ; body is a single element (not list)
      (cond
        [(equal? (first lst-param-body) (second lst-param-body)) arg] ;body is just 1 param
        [else (second lst-param-body)] ;body is literal
        )
      )
  )
