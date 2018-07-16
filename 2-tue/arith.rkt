#lang racket
(require (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [literal #%datum]
                     [plus +]
                     [if-macro if]
                     [complain-app #%app]
                     [unwrap #%top-interaction])
         then
         else)
 
(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . v:number) #'(#%datum . v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . s:string) #'(quote s)]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))
 
(define-syntax (plus stx)
  (syntax-parse stx
    [(_ n1 n2) #'(+ n1 n2)]))

(define-syntax (if-macro stx)
  (syntax-parse stx
    #:literals  (then else)
    [(_ guard:expr then e1:expr else e2:expr)
     #'(if guard e1 e2)]))

(define-syntax (then stx)
  (syntax-parse stx
    [(_) (raise-syntax-error #f "not allowed")]))

(define-syntax (else stx)
  (syntax-parse stx
    [(_) (raise-syntax-error #f "not allowed")]))

 
(define-syntax (complain-app stx)
  (define (complain msg src-stx)
    (raise-syntax-error 'parentheses msg src-stx))
  (define without-app-stx
    (syntax-parse stx [(_ e ...) (syntax/loc stx (e ...))]))
  (syntax-parse stx
    [(_)
     (complain "empty parentheses are not allowed" without-app-stx)]
    [(_ n:number)
     (complain "extra parentheses are not allowed around numbers" #'n)]
    [(_ x:id _ ...)
     (complain "unknown operator" #'x)]
    [_
     (complain "something is wrong here" without-app-stx)]))

(define-syntax (unwrap stx)
  (syntax-parse stx
    [(_ . e) #'e]))