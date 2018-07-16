#lang s-exp "typed-lang.rkt"
; e.g., save this to file "morn-typed-prog.rkt"
5
#f
"five"
; 1.1 ;; err
; (if 1 2 3) ;; err
(if #f 2 3)
+
(+ 1 2)
; (+ #f 1) ;; err
(lambda ([x : Int]) (+ 1 x))
(lambda ([x : Int])
  (lambda ([y : String])
    x))
(lambda ([x : Bool] [y : String]) (if x y "no"))

((lambda ([x : Int]) x) 1)

;((lambda ([x : Bool] [y : String]) (if x y "no")) #t "ciao")


; Running the program prints:
; 5 : Int
; #f : Bool
; five : String
; (if #f 2 3) : Int
; + : (-> Int Int Int)
; (+ 1 2) : Int