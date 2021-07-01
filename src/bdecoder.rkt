#lang racket

(require "util.rkt")

(provide bdecode)

(define (bdecode str)
  (let* (;[str (open-input-string s)]
         [c (peek-char str)])
    (cond
      [(char=? c #\i) (bdecode_int str)]
      [(char=? c #\d) (bdecode_dic str)]
      [(char=? c #\l) (bdecode_lst str)]
      [(char-numeric? c) (bdecode_str str)]
      [else (display "pute\n") (displayln c)]
     )))

(define (bdecode_int s)
  ; skips 'i'
  (read-char s)
  (stoi (list->string (for/list ([c (in-input-port-chars s)]
             #:break (char=? c #\e))
    c))))

(define (bdecode_str s)
  (let ([length (stoi (list->string (for/list ([c (in-input-port-chars s)]
                                               #:break (not (char-numeric? c)))
                                      c)))])
    (read-bytes length s)))

(define (bdecode_lst s)
  ; skips 'l'
  (read-char s)
  (let ([res (for/list ([c (in-port peek-char s)]
             #:break (char=? c #\e))
    (bdecode s))])
        (read-char s)
    res))

(define (bdecode_dic s)
  ; skips 'd'
  (read-char s)
  (let ([res (for/list ([c (in-port peek-char s)]
             #:break (char=? c #\e))
    (cons (bdecode_str s) (bdecode s)))])
    (read-char s)
    res))
