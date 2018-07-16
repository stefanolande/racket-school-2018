#lang racket

#|
  Definition	= (define-function (Variable Variable1 ...)  Expression)
 	 	 	 	 
  Expression	= (function-application Variable Expression ...)
 	 	| (if Expression Expression Expression)
 	 	| (+ Expression Expression)
 	 	| Variable
 	 	| Number
 	 	| String
|#

(require (for-syntax syntax/parse racket/list))

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (function-name:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax function-name
         (cons #,arity #'(lambda (parameter ...) body)))]))

(define-syntax plus
  (cons 2 #'(lambda (x y) (+ x y))))

(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

(define-syntax (function-app stx)
  (syntax-parse stx
    [( _ function-name:id argument:expr ...)     
     #:do ((define-values (arity function) (lookup #'function-name stx)))
     #:fail-unless (= arity (length (syntax->list #'(argument ...)))) "wrong number of arguments"
     #`(#,function argument ...)]))

#|
#;
(define-syntax (plus stx)
  (syntax-parse stx
    [( _ args:expr ...)     
     #:fail-unless (= 2 (length (syntax->list #'(args ...)))) "plus expects 2 arguments"
     #`(+ args ...)]))

(define-syntax (define-predef-fun stx)
  (syntax-parse stx
    [(_ (the-id:id args:expr ...) body:expr)
     (define arg-n (length (syntax->list #'(args ...))))
     #`(define-syntax (the-id stx)
          (syntax-parse stx
            [(_ args ... argn (... ...))
             #:fail-unless (empty? (syntax->list #'(argn (... ...)))) (format "~a expects ~a arguments" 'the-id #,arg-n)
             #'body]))]))

(define-predef-fun
  (plus x y) (+ x y))

(define-predef-fun
  (minus x y) (- x y))

(define-syntax (string+ stx)
  (syntax-parse stx
    [( _ args:expr ...)     
     #:fail-unless (= 2 (length (syntax->list #'(args ...)))) "string+ expects 2 arguments"
     #:fail-unless (andmap string? (syntax->list #'(args ...))) "ccc"
     #`(string-append args ...)]))


(string+ "a" "b")
(string+ "a" 1)
|#

(function-app plus 1 2)