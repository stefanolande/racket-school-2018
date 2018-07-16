#lang racket

(require (for-syntax syntax/parse))
(require rackunit)

(define-syntax (some stx)
  (syntax-parse stx
    [(_ e0:expr)
     #'(let ([e0-val e0])
        (and e0-val (list e0-val)))]
    [(_ e0:expr e1:expr ...)
     (combine #'e0 #'(some e1 ...))]))

(begin-for-syntax
  (define (combine e0 some-of-e1)
    #`(let ([v #,e0])
        (if v
            (let ([w #,some-of-e1])
              (if (cons? w)
                  (cons v w)
                  (list v)))
            #f)))
)

(check-equal? (some #f) #f)
(check-equal? (some 1) (list 1))
(check-equal? (some (begin (displayln "hello") 1)) (list 1))

(check-equal? (some 1 #f) (list 1))
