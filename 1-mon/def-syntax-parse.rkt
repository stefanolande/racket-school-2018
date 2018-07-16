#lang racket

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

(define-syntax (define-hello stx)
  (syntax-parse stx
    [(_ the-id:id ...)
     #'(define-values (the-id ...) (values (begin 'the-id "good bye")...))]))

(define-hello world day night)
world
day
night