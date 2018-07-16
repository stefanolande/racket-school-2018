#lang turnstile/quicklang
; e.g., save this to file "typed-lang.rkt"

(provide Int String ->
         (rename-out [typed-app #%app]
                     [typed-+ +]
                     [typed-datum #%datum])) 
 
(define-base-types Int String)
(define-type-constructor -> #:arity > 0)
 
(define-primop typed-+ + : (-> Int Int Int))
 
(define-typerule (typed-app f e ...) ≫
[⊢ f ≫ f- ⇒ (~-> τin ... τout)]
[⊢ e ≫ e- ⇐ τin] ...
--------------------
[⊢ (#%plain-app f- e- ...) ⇒ τout])
 
(define-typerule typed-datum
[(_ . n:integer) ≫
 -------------
 [⊢ (#%datum . n) ⇒ Int]]
[(_ . s:str) ≫
 -------------
 [⊢ (#%datum . s) ⇒ String]]
[(_ . x) ≫
 --------
 [#:error (type-error #:src #'x #:msg "Unsupported literal: ~v" #'x)]])
