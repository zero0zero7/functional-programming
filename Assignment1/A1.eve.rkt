#lang racket ; CSC324 — 2023W — Assignment 1 — Eve Implementation

; Task: implement eve according to A1.eve-test.rkt.

(provide
 (contract-out (eve (any/c . -> . (list/c any/c (hash/c symbol? list?)))))
 ; Add any helper functions you tested in A1.eva-test.rkt.
 ; Whether you add contracts to them is optional.
 #;a-helper
 set-and-ref)

; · Support: indexer

; A constructor for a zero-arity function that when called successively
; produces symbols of the form prefix0 prefix1 prefix2 etc.

(provide (contract-out (indexer (any/c . -> . (-> symbol?)))))

(define (indexer prefix)
  (define last-index -1)
  (λ ()
    (local-require (only-in racket/syntax format-symbol))
    (set! last-index (add1 last-index))
    (format-symbol "~a~a" prefix last-index)))

; · eve

; Produce a two-element list with the value and the environment-closure table
; from evaluating an LCE term.
(define (eve term)

  ; A mutable table of environments and closures.
  ; Task: Look up hash-ref and hash-set! in the racket documentation.
  (define environments-closures (make-hash))
  
  ; Iterators to produce indices for environments and closures.
  (define En (indexer 'E))
  (define λn (indexer 'λ))

  ; Task: complete rec.
  ;(define (rec t E) t)
  (define (rec t E)
    (match t
      ;lambda term
      [(list 'λ (list param) body) (let ([lambda-n (λn)])
                                     (set-and-ref environments-closures lambda-n t E))]
      ;func-call term
      [(list func arg) (let ([func-result (rec func E)] [arg-result (rec arg E)])
                         (let ([func-hash (hash-ref environments-closures func-result)])
                           (match func-hash
                             [(list (list 'λ (list param) body) old-env)
                              (let ([new-env (En)])
                                (hash-set! environments-closures new-env (list (list param arg-result) old-env)) (rec body new-env))]
                             )))]
      ;variable or value: hash-ref the environment
      ; if successfully referenced ie. got list, obtain the binding, if binding's var matches term, return, else recurse to the oldenv
      [var-val (if (list? (hash-ref environments-closures E var-val))
                   (cond
                     [(equal? (first(first(hash-ref environments-closures E))) var-val) (second(first(hash-ref environments-closures E)))]
                     [(equal? (second(hash-ref environments-closures E)) 'E0) var-val]
                     [else (rec var-val (second(hash-ref environments-closures E)))])
                   var-val)] 
    ))
  
  (list (rec term (En))
        (make-immutable-hash (hash->list environments-closures))))


(define (set-and-ref hash key lambda-term env)
  ;key = lambda-index
  (hash-set! hash key (list lambda-term env))
  key)