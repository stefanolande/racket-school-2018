#lang racket

(require (for-syntax syntax/parse))
(require rackunit)
(require (for-syntax racket/list))

;ex 1

(define-syntax (define-world* stx)
  (define size (sub1 (length (syntax->list stx))))
  (define param (range size))
  (syntax-parse stx
    [(_ the-id:id ...)
     #`(define-values (the-id ...) (values #,@param))]))

(define-syntax (define-world*2 stx)
  (syntax-parse stx
    [(_ x ...)
     #`(begin
         (define counter -1)
         (define x (begin (set! counter (add1 counter)) counter))
         ...)]))

(define-world*2 x y z)
(check-equal? (list x y z) (list 0 1 2))

;ex 5

(define-syntax (define-rewrite-rule stx)
  (syntax-parse stx
    [(_ (the-id:id the-exp:expr ...) body:expr)
     #'(define-syntax (the-id stx)
         (syntax-parse stx
           [(_ the-exp ...)
            #'body]))]))


(define-rewrite-rule
  (loop-for-ever exp)
  ; —> 
  (local ((define (for-ever) (begin exp (for-ever)))) (for-ever)))


(define-rewrite-rule
  (ciao n)
  (add1 n))

;ex 3
(define-rewrite-rule
  (all exp ...)
  (if (and exp ...)
      (list exp ...)
      #f))





(define bindings (syntax->