#lang algebra

(define-function (add1 x)
  (+ x 1))

(function-application add1 1)

(if #t then "ciao" else 1)

(- 5 1)
(* 5 3)
(/ 10 2)

(+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 0.1)))))))))

(exactly 2.0)
(inexactly (+ 1.0 (exactly (/ 1.0 0))))