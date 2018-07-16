#lang racket

(require (for-syntax syntax/parse))
(require rackunit)

(define-syntax-rule
  (where body [iden val] ...)
  (let ([iden val] ...)
    body))

(check-equal?
 (where (+ my-favorite-number 2)
        [my-favorite-number 8])
 10)

(check-equal?
 (where (op 10 (+ my-favorite-number an-ok-number))
        [my-favorite-number 8]
        [an-ok-number 2]
        [op *])
 100)

(define-syntax (where* stx)  
  (syntax-parse stx
    [(_ body [iden val] ...)
     (define bindings (reverse (syntax->list #'([iden val] ...))))
     #`(let* #,bindings
         body)]))

(check-equal?
 (where* (list x y z)
         [x (+ y 4)]
         [y (+ z 2)]
         [z 1])
 (list 7 3 1))

;;;
;; exercise 7