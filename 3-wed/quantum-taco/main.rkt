#lang br/quicklang

(module+ reader
  (provide read-syntax))

#;
(define (tokenize ip)
  (let ([tk (read ip)])
    (if (eof-object? tk)
        empty
        (cons tk (tokenize ip)))))

(define (tokenize ip)
  (for/list ([tok (in-port read ip)])
    tok))

(define (parse toks)
  (if (list? toks)
      (map parse toks)
    'taco))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (with-syntax ([(PT ...) parse-tree])
    #'(module tacofied racket
        'PT ...)))