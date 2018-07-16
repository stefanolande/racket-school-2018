#lang br/quicklang

(require racket/format)

(module+ reader
  (provide read-syntax))

(define (number->taco n)
  (if (equal? n "0")
      "()"
      "taco"))

(define (char->bits c)
  (define n (char->integer c))
  (cond [(< n 2) (number->string n)]
        [else (string-append (char->bits (integer->char (quotient n 2))) (number->string (remainder n 2)))]))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
    tok))

(define (stringreverse s)
  (list->string (reverse (string->list s))))

(define (tacofystring s)
  (string-join (for/list([c s])
    (if (equal? c #\1)
        "taco"
        "()"))))

(define (parse toks)
  (define bytes (map char->bits toks))
  (define bytes-pad-rev (map (lambda (s) (tacofystring (stringreverse (~a s #:min-width 7 #:left-pad-string "0" #:align 'right)))) bytes))
  (append bytes-pad-rev)
  )

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module tacofied racket
         (for([s 'PT])
         (displayln (string-append "(" s ")")))
         ))))
