#lang racket ; CSC324 — 2023W — Assignment 1 — Eve — Design and Testing

; • Eve: Eager By-Value Environmental Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCE described below,
; then create a good test suite for an evaluator eve of LCE which you will then
; implement in A1.eve.rkt

(require "A1.eve.rkt"
         rackunit)

; We'll refer to the language being interpreted in this part as “LCE”.

; · Terms in LCE

; LCE has the same terms as LCA, with the same representation.

; · Values, Closures, and Environments in LCE

; A closure pairs a (not necessarily closed) λ term with an environment that
; contains (at least) bindings for the open variables in the λ term.

; An environment pairs a local binding of a variable and value with a parent
; environment.

; For concision, closures and environments will be kept in a hash table to be
; referenced by symbolic indices of the form "λn" and "En".

; So a closure is represented by a two-element list containing a λ term and an
; environment index.

; And an environment is represented by a two-element list containing a
; two-element list of variable and value along with an environment index.

; A value is then one of:
;   closure index
;   literal term

; · Semantics of LCE

; Evaluation now takes both a term and the index of an environment that contains
; (at least) bindings for the open variables in the term.

; Evaluation of a value is still just the value itself.

; Evaluation of a variable is its most local value starting from the environment
; and continuing up the parent chain.
; Evaluation Assumption: the variable has a binding in the chain.

; Evaluation of function call is still eager and by value, but with argument
; passed by environment.
; More precisely, evaluation of (<function-term> <argument-term>) is:
;   1. Evaluate <function-term> in the environment.
;      Evaluation Assumption: the evaluation is not an infinite recursion
;      and the result is the index of a closure with λ term (λ (<id>) <body>))
;      and environment E.
;   2. Evaluate <argument-term> in the environment to produce a value v.
;      Evaluation Assumption: the evaluation is not an infinite recursion.
;   3. Produce the evaluation of <body> in a new environment extending E with
;      a local binding of <id> to v.

; Evaluation of a λ term is the index of a new closure containing the term and
; environment index.

; · Design and Testing

; Create a good test suite for eve, with a comment above each test case giving
; a rationale for its inclusion.

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.eve.rkt so they can be referenced here.

; Illustrating indexer.
(define s (indexer 'p))
; A result of indexer produces successive indices starting at 0.
(check-equal? (s) 'p0)
(check-equal? (s) 'p1)
(check-equal? (s) 'p2)
; Warning: the result is not referentially transparent!
(check-not-equal? (s) (s))

; Illustrative example for the form of an environment-closure table.
; The test also illustrates the literal notation for (immutable) hash tables.
; In A1.eve.rkt you update a mutable hash table, but it's converted to immutable
; just before return (an immutable hash table and a mutable hash table are
; always considered unequal).
(check-equal? (eve '(((λ (x) (λ (y) 1)) 20) 300))
                '(1 #hash((E1 . ((x 20) E0))
                          (E2 . ((y 300) E1))
                          (λ0 . ((λ (x) (λ (y) 1)) E0))
                          (λ1 . ((λ (y) 1) E1)))))

; 3 more simple tests to help you to start your test suite and implementation

; Literal number.
(check-equal? (eve 324) '(324 #hash()))

; λ in an environment.
(check-equal?  (eve '(λ (x) 324))
         '(λ0 #hash((λ0 . ((λ (x) 324) E0)))))

(check-equal? (eve '((λ (x) 324) (λ (y) 325)))
                '(324 #hash((E1 . ((x λ1) E0))
                   (λ0 . ((λ (x) 324) E0))
                   (λ1 . ((λ (y) 325) E0)))))

;NEW TEST CASES
; Literal value, specifically a string
(check-equal? (eve "literal string") '("literal string" #hash()))

; λ-term where the body is another λ-term whose body is a literal
(check-equal? (eve '(λ (x) (λ (y) 1)))
              '(λ0 #hash((λ0 . ((λ (x) (λ (y) 1)) E0)))))
; λ-term where body is a function call
(check-equal? (eve '(λ (y) ((λ (x) x) "argument value")))
              '(λ0 #hash((λ0 . ((λ (y) ((λ (x) x) "argument value")) E0)))))
; function call with function'term's parameter being an identifier with binding to the argument-term
(check-equal? (eve '((λ (x) 324) "value passed into x"))
                   '(324 #hash((E1 . ((x "value passed into x") E0))
                               (λ0 . ((λ (x) 324) E0)))))
; function call where the function-term's body is another function call.
(check-equal? (eve '((λ (x) (λ (y) x)) "value passed into x"))
                    '(λ1 #hash((E1 . ((x "value passed into x") E0))
                               (λ0 . ((λ (x) (λ (y) x)) E0))
                               (λ1 . ((λ (y) x) E1)))))
; function call where the function-term is a lambda-term without argument, and argument-term is another lambda term with argument
(check-equal? (eve '((λ (x) 324) ((λ (y) 325) "value passed into y")))
                '(324 #hash((E1 . ((y "value passed into y") E0))
                            (E2 . ((x 325) E0))
                            (λ0 . ((λ (x) 324) E0))
                            (λ1 . ((λ (y) 325) E0)))))
; similar to above, but additional layer of nesting withing the argument-term
(check-equal? (eve '((λ (z) z) ((λ (y) y) ((λ (x) x) "x"))))
              '("x" #hash((E1 . ((x "x") E0))
                        (E2 . ((y "x") E0))
                        (E3 . ((z "x") E0))
                        (λ0 . ((λ (z) z) E0))
                        (λ1 . ((λ (y) y) E0))
                        (λ2 . ((λ (x) x) E0)))))

;TEST CASES FOR HELPER FUNCTION set-and-ref
(check-equal? (set-and-ref (make-hash) 1 '(λ (x) x) 'E0) 1)
(check-equal? (set-and-ref (make-hash) "key" '(λ (x) x) 'E0) "key")


#|; function call where the function is a lambda term with argument, and argument is another lambda term with argument
(check-equal? (eve '(((λ (x) 324) "value passed into x") ((λ (y) 325) "value passed into y")))
                '(324 #hash((E1 . ((x "value passed into x") E0))
                            (E2 . ((y "value passed into y") E0))
                            (E3 . ((x "value passed into y") E2))
                            (λ0 . ((λ (x) 324) E0))
                            (λ1 . ((λ (y) 325) E0)))))|#