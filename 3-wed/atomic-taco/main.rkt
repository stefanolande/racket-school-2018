#lang racket

(require (for-syntax syntax/parse))
(provide
 (rename-out [taco-datum #%datum]
             [taco-app #%app]
             [taco-datum %app])
 #%module-begin
 #%top-interaction)

(module reader syntax/module-reader
  atomic-taco)

(define-syntax (taco-datum stx)
  (syntax-parse stx
    [(_  . a) #'(#%datum . taco)]))


(define-syntax (taco-app stx)
  (syntax-parse stx
    [(_ id a ...)  #'`(taco ,a ...)]))

