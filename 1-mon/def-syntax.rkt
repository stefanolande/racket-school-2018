#lang racket

(require (for-syntax racket/list))

(define-syntax (define-hello stx)
  (define id (second (syntax->list stx)))
  #`(define #,id "good bye"))

(define-hello world)
world