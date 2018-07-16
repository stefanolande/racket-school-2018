
#lang racket
; e.g., save this to file "morn-typed-lang.rkt"
(require (for-syntax syntax/parse syntax/stx syntax/id-table racket/match))
(provide (rename-out [typechecking-mb #%module-begin])
         + if lambda)



 
(begin-for-syntax

  ; A TyEnv is a [ImmutableFreeIdTableOf IdStx -> TyStx]
 
  ; mk-empty-env : -> TyEnv
  ; Returns a new type environment with no bindings.
  (define (mk-empty-env)
    (make-immutable-free-id-table))	
 
  ; add-to-env : TyEnv IdStx TyStx -> TyEnv
  ; Returns a new type environment that extends the given env with the given binding.
  (define (add-to-env env id type)
    (free-id-table-set env id type))

  (define (add-list-to-env env ident-list type-list)
    (match* (ident-list type-list)
      [((list-rest ident id-res) (list-rest type t-res)) (add-list-to-env (add-to-env env ident type) id-res t-res)]
      [((list ) (list )) env]))
  
  ; lookup-env : TyEnv IdStx -> TyStx or #f
  ; Looks up the given id in the given env and returns corresponding type. Returns false if the id is not in the env.
  (define (lookup-env env id)
    (free-id-table-ref env id (lambda () (raise-syntax-error
                                          'compute
                                          (format "unbound identifier ~a" (syntax->datum id))))))
 
  ; A TyStx is a syntax object representing a type.
  ; A ExprStx is a syntax object representing an expression.
  ; A IdStx is an identifier syntax object.
 
  ; TODO: complete this function
  ; compute: ExprStx -> TyStx
  ; computes the type of the given term
  (define (compute env e)
    (syntax-parse e
      [:integer #'Int]
      [:string #'String]
      [:boolean #'Bool]
      [((~literal if) e1 e2 e3)
       #:do [(define t (compute env #'e2))]
       #:when (and (check env #'e1 #'Bool) (check env #'e2 t))
       t]
      [((~literal +) e1 e2)
       #:when (and (check env #'e1 #'Int) (check env #'e2 #'Int))
       #'Int]
      [(~literal +) #'(-> Int Int Int)]
      [x:id (lookup-env env #'x)]
      [((~literal lambda) ((var:id (~literal :) type:expr) ...) body:expr)
       (define new-env (add-list-to-env env (syntax->list #'(var ...)) (syntax->list #'(type ...))))
       (define ret-type (compute new-env #'body))
       #`(-> type ... #,ret-type)]
      [(fun e1 ...) ;function application
       #:with ((~literal ->) t1 ... t2) (compute env #'fun)
       #:fail-unless (= (length (syntax->list #'(e1 ...))) (length (syntax->list #'(t1 ...)))) "arity error"
       #:when (andmap (lambda (e t) (check env e t))
                      (syntax->list #'(e1 ...))
                      (syntax->list #'(t1 ...)))
       #'t2
       ]
      [e (raise-syntax-error
          'compute
          (format "could not compute type for term: ~a" (syntax->datum #'e)))]))
 
  ; check : ExprStx TyStx -> Bool
  ; checks that the given term has the given type
  (define (check env e t-expected)
    (define t (compute env e))
    (or (type=? t t-expected)
        (raise-syntax-error
         'check
         (format "error while checking term ~a: expected ~a; got ~a"
                 (syntax->datum e)
                 (syntax->datum t-expected)
                 (syntax->datum t)))))
 
  ; type=? : TyStx TyStx -> Bool
  ; type equality here is is stx equality
  (define (type=? t1 t2)
    (or (and (identifier? t1) (identifier? t2) (free-identifier=? t1 t2))
        (and (stx-pair? t1) (stx-pair? t2)
             (= (length (syntax->list t1))
                (length (syntax->list t2)))
             (andmap type=? (syntax->list t1) (syntax->list t2))))))
 
(define-syntax typechecking-mb
  (syntax-parser
    [(_ e ...)
     ; prints out each term e and its type, it if has one;
     ; otherwise raises type error
     #:do[(stx-map
           (λ (e)
             (printf "~a : ~a\n"
                     (syntax->datum e)
                     (syntax->datum (compute (mk-empty-env) e))))
           #'(e ...))]
     ; this language only checks types,
     ; it doesn't run anything
     #'(#%module-begin (void))]))