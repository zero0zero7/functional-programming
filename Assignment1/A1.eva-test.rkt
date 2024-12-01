#lang racket ; CSC324 — 2023W — Assignment 1 - Eva Design and Testing

; • Eva: Eager By-Value Algebraic Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCA described below,
; then create a good test suite for an evaluator of LCA named eva which you will
; then implement in A1.eva.rkt

(require "A1.eva.rkt"
         rackunit)

; · Syntax of LCA

; The grammar of “terms” [along with how we'll refer to them] in LCA is:
;   term = (λ (<parameter-identifier>) <body-term>)  [“λ term”]
;        | (<function-term> <argument-term>)  [“function call term”]
;        | <identifier>  [“variable term”]
;        | <literal>  [“literal term”]

; A term where each variable term <id> occurs inside some enclosing λ term
; whose parameter is <id> is called “closed”, otherwise it's called “open”.
; In particular, identifier terms are open and literal terms are closed.

; We will refer to literal terms and closed λ terms (closures) as “values”.

; Parenthesized terms are represented by lists.
; Identifiers (including "λ") are represented by symbols.
; Literals are, and represented by, values that are not lists nor symbols.

; For example, the following represents a (syntactically) valid term in LCA,
; and contains one instance of each production:
#;'(324 (λ (y) y))
; It contains two values:
#;(λ (y) y)
#;324

; · Semantics of LCA

; Assume that the evaluator is only asked to evaluate closed terms that are
; semantically valid (satisfy the evaluation assumptions mentioned below).
; This ends up guaranteeing that the recursive evaluation described below
; only produces values (exercise: prove this by structural induction).

; Evaluation of a value is just the value itself.

; Evaluation of function call is eager algebraic substitution of argument value.
; More precisely, evaluation of (<function-term> <argument-term>) is:
;   1. Evaluate <function-term>.
;      Evaluation Assumption: the evaluation is not an infinite recursion
;      and the result is a closed λ term (λ (<id>) <body>).
;   2. Evaluate <argument-term> to produce a value v.
;      Evaluation Assumption: the evaluation is not an infinite recursion.
;   3. Substitute occurrences of variable term <id> in <body> with v.
;      Substitution respects scope: if <body> contains a λ term whose parameter
;      is also <id> it does not replace <id> inside that λ term.
;   4. Produce the evaluation of the transformed body.

; · Design and Testing

; Create a good test suite for eva, with a comment above each test case giving
; a clear rationale for its inclusion. We have provided you with 8 test cases
; to help start the test suite and the implementation. 

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.eva.rkt so they can be referenced here.

; A literal term.

(check-equal? (eva 324) 324)
(check-equal? (eva 'eva) 'eva)
(check-equal? (eva '(λ (x) x)) '(λ (x) x))

; Call literal λ having literal body, with literal argument.
; Call literal λ with body substitution to literal.
; Call literal λ with body requiring evaluation.


(check-equal? (eva '((λ (y) 325) 324)) 325)
(check-equal? (eva '((λ (y) y) 324)) 324)
(check-equal? (eva '((λ (y) (λ (x) 324)) 325)) '(λ (x) 324))
(check-equal? (eva '((λ (x) x) ((λ (x) 325) 324))) 325)
(check-equal? (eva '((λ (x) (x x)) (λ (y) 324))) 324)

;NEW TEST CASES
; Call literal λ having literal body requiring substitution
(check-equal? (eva '((λ (y) ((λ (x) x) y)) 324)) 324)
; Evaluating function call where argument-term is another function call that requires substitution
(check-equal? (eva '((λ (y) y) ((λ (x) x) 325)))325)
; Evaluating function call where argument-term is a literal λ-term
(check-equal? (eva '((λ (y) y) (λ (x) x))) '(λ (x) x) )
; Evaluating function call where function-term is a literal λ-term
(check-equal? (eva '((λ (y) (λ (x) x)) 325)) '(λ (x) x) )
; Evaluating function call where function-term isanother function call
(check-equal? (eva (((λ (y) y) (λ (x) x)) 325)) 325 )

;TEST CASES FOR HELPER FUNCTION eva-func
(check-equal? (eva-func '(λ (param) body)) '(param body))
(check-equal? (eva-func '(λ (param) (λ (param2) body))) '(param (λ (param2) body)))
;TEST CASES FOR HELPER FUNCTION eva-func-call
; body is a list with element matching parameter
(check-equal? (eva-func-call (list 'y (list 'λ (list 'y) 'y)) 2) '(λ (y) 2))
; body is a list with element not matching parameter
(check-equal? (eva-func-call (list 'y (list 'λ (list 'y) 'x)) 2) '(λ (y) x))
; body is just a parameter
(check-equal? (eva-func-call (list 'y 'y) 2) 2)
; body is just a literal
(check-equal? (eva-func-call (list 'y 1) 2) 1)