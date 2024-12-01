#lang racket

; • CSC324 — 2023W — Exercise 1

; Due: Mon Jan 30th 5PM.

; Implement the seven functions exported in the "provide" statement below, which
; are described in more detail later in the file.

; You may add more tests.

; The following tracing is worthwhile.
; Use a version of trace-functions.rkt from the course.
; Note: anonymous functions tracing is now optional, indicated by keyword
; #:λ or #:lambda.
(require "trace-functions.rkt")
(show-call n-of.loop cat.rec rec.every? #:λ)
(show-sub #:λ)

; Showing definitions as variable definition and function creation is optional.
#;(show-expansion #t)

; Each export has a “contract” attached, to blame the (external) caller if they
; call the function with arguments of the wrong type, or blame the function if
; it returns a value of the wrong type.
(provide
 (contract-out [n-of       (natural? any/c . -> . (listof any/c))]
               [rec.n-of   (natural? any/c . -> . (listof any/c))]
               [cat        (natural? string? . -> . string?)]
               [rec.cat    (natural? string? . -> . string?)]
               [every?     (list? (any/c . -> . boolean?) . -> . boolean?)]
               [rec.every? (list? (any/c . -> . boolean?) . -> . boolean?)]
               [parts      (any/c . -> . list?)]
               ))

; To learn to program in and compare multiple styles, from racket use only the
; following imported identifiers ...
#;{; Syntactic Forms
   define ; definitions
    λ lambda ; anonymous functions
    if and or ; conditionals    
    ; Variables (all but one of these refer to a function)
    map apply filter compose; higher-order functions
    equal? = ; equality predicates, = is only numeric
    not ; boolean predicate
    number? boolean? list? string? procedure? ; type predicates
    natural? zero? positive? negative? > < >= <= ; numeric predicates
    add1 sub1 + -  ; arithmetic functions
    list cons list* empty append range ; list construction
    empty? ; list predicate
    first rest ; list accessors
    length ; list property
    string-append ; string construction
    }
; ... and follow the approaches as described.
; Make sure you understand the PL terminology used to describe those approaches
; — ask us about PL terminology if you're unsure!

(module+ test (require rackunit))

; · n-of / rec.n-of

; Produce a list of n copies of a value v.
#;(n-of n v)
#;(rec.n-of n v)


(module+ test
  (check-equal? (n-of 0 324) (list))
  (check-equal? (n-of 3 324) (list 324 324 324)))
#;
(module+ test
  (check-equal? (rec.n-of 0 324) (list))
  (check-equal? (rec.n-of 3 324) (list 324 324 324)))

; For n-of : map an anonymous unary constant function onto a range.

; For rec.n-of : review scroll-tail from the lab, then complete the nested
; helper function as a “tail recursion” that ”accumulates” the result in the
; second argument on the way in to the recursion, i.e.
; > (rec.n-of 2 "cat")
; — (n-of.loop 2 '())
; ——— (n-of.loop 1 '("cat"))
; ————— (n-of.loop 0 '("cat" "cat"))
; ————— '("cat" "cat")
; ——— '("cat" "cat")
; — '("cat" "cat")
; '("cat" "cat")


(define (n-of n v)
  (map (lambda (x) v) (range n))
  )

(define (rec.n-of n v)
  (define (helper n v l)
  (if (zero? n)
      l
  (helper (- n 1) v (append l (list v)))))
  (helper n v '())
  )


; · cat / rec.cat

; Produce the string containing n copies of string s.
#;(cat n s) #;(rec.cat n s)

 (module+ test
  (check-equal? (cat 3 "cat") "catcatcat")
  (check-equal? (cat 0 "cat") ""))

; For cat : call n-of and a higher-order function.
; For rec.cat : review scroll from the lab, then complete the nested helper
; function as a “primitive recursion” that computes the result from a smaller
; version of the original problem, i.e.
; > (rec.cat 2 "cat")
; — (cat.rec 2)
; ——— (cat.rec 1)
; ————— (cat.rec 0)
; ————— ""
; ——— "cat"
; — "catcat"

(define (cat n s)
  (string-join (n-of n s)  ""
               #:before-first ""
               #:before-last ""
               #:after-last "")
  )
  

(define (rec.cat n s)
  (if (zero? n)
      ""
      (string-append s (rec.cat (- n 1) s )))
  )

; · every? / rec.every?

; Whether every element of list a-list satisfies unary predicate p?.
#;(every? a-list p?) #;(rec.every? a-list p?)

(module+ test
  (check-equal? (every? (list) even?) #t)
  (check-equal? (every? (list 324 165) even?) #f))


(module+ test
  (check-equal? (rec.every? (list) even?) #t)
  (check-equal? (rec.every? (list 324 165) even?) #f))

; For every? : use filter and examine the result.

; For rec.every? : use primitive recursion on the rest of the list,
; combining with or and and, without using if nor a nested helper function.

(define (every? a-list p?)
  (if (equal? (filter p? a-list) a-list)
      #t
  #f)
  )


(define (rec.every? a-list p?)
  (if (empty? a-list)
      #t
      (and (rec.every? (cdr a-list) p?) (p? (car a-list))))
  )

; · parts

; Take a value and produce a list of all its “parts”: itself, and if the value
; is a list then all the parts of its elements.


(module+ test
  (check-equal? (parts 324) (list 324))
  (check-equal? (parts (list)) (list (list)))
  (check-equal?
   (parts (list "ape" (list (list "bug" "cow") "doe") (list "emu")))
   '(("ape" (("bug" "cow") "doe") ("emu"))
     "ape"
     (("bug" "cow") "doe") ("bug" "cow") "bug" "cow" "doe"
     ("emu") "emu"))
  (check-equal?
   (parts (list "ape" (list (list "bug" "cow") "doe") (list "emu")))
   (cons (list "ape" (list (list "bug" "cow") "doe") (list "emu"))
          (append (list "ape")
                  (list (list (list "bug" "cow") "doe")
                        (list "bug" "cow")
                        "bug"
                        "cow"
                        "doe")
                  (list (list "emu") "emu")))))

(define (parts v)
  (void))

