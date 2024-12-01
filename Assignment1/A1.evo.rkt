#lang racket ; CSC324 — 2023W — Assignment 1 — Evo Implementation

; Task: implement evo according to A1.evo-test.rkt.

(provide
 (contract-out (evo (any/c . -> . (list/c any/c (hash/c symbol? list?)))))
 ; Add any helper functions you tested in A1.evo-test.rkt.
 ; Whether you add contracts to them is optional.
 set-and-ref)

; · Support: indexer and Void

(provide (contract-out (indexer (any/c . -> . (-> symbol?)))))

(define (indexer prefix)
  (define last-index -1)
  (λ ()
    (local-require (only-in racket/syntax format-symbol))
    (set! last-index (add1 last-index))
    (format-symbol "~a~a" prefix last-index)))

; There is no literal nor variable for the void value (although it has a printed
; representation when printed inside compound values), so we'll name it here and
; and also export it for testing.

(provide (contract-out (Void void?)))

(define Void (void))

; · evo

; Produce a two-element list with the value and the environment-closure table
; from evaluating an LCO term.
(define (evo term)

  ; A mutable table of environments and closures.
  (define environments-closures (make-hash))
  
  ; Iterators to produce indices for environments and closures.
  (define En (indexer 'E))
  (define λn (indexer 'λ))

  ; Task: complete rec.
  ;(define (rec t E) t)
  (define (rec t E)
    (match t
      ;lambda term without result-term
      [(list 'λ (list param) body ...) (let ([lambda-n (λn)])
                                     (set-and-ref environments-closures lambda-n t E))]
      ;lambda term with result-term
      [(list 'λ (list param) body ... result) (let ([lambda-n (λn)])
                                     (set-and-ref environments-closures lambda-n t E))]
      
      ;func-call term                        
      [(list func arg) (let ([func-result (rec func E)] [arg-result (rec arg E)])
                         (if (procedure? func-result)
                             (func-result arg-result)
                             (let ([func-hash (hash-ref environments-closures func-result)])
                               (match func-hash
                                 [(list (list 'λ (list param) body ... result) old-env)                     
                                  (let ([new-env (En)])
                                    (hash-set! environments-closures new-env (list (list param (box arg-result)) old-env))
                                    (rec body new-env)
                                    (rec result new-env)
                                    )]
                                 ))
                             )
                         )]
      
      ;assignment term
      [(list set! var update) (let ([update-result (rec update E)]) ;evaluate update-term in current env
                                (if (hash-has-key? environments-closures E)
                                    ;update the variable binding in current env: hash-ref to get env, first to get variable binding, set-box!
                                    (set-box! (second(first(hash-ref environments-closures E))) update-result)
                                    (hash-set! environments-closures (En) (list (list var (box update-result)) E))
                                    )
                                Void)]
      ;variable or value: hash-ref the environment
      ; if successfully referenced ie. got list, obtain the binding, if binding's var matches term, return, else recurse to the oldenv
      [var-val (if (hash-has-key? environments-closures E)
                   (cond
                     [(equal? (first(first(hash-ref environments-closures E))) var-val)
                      (unbox (second(first(hash-ref environments-closures E))))]
                     [(equal? (second(hash-ref environments-closures E )) 'E0) var-val]
                     [else (rec var-val (second(hash-ref environments-closures E)))])
                   var-val)]
      ))
  
  (list (rec term (En))
        (make-immutable-hash (hash->list environments-closures))))


(define (set-and-ref hash key lambda-term env)
  ;key = lambda-index
  (hash-set! hash key (list lambda-term env))
  key)