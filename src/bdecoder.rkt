#lang racket

(require "util.rkt")

(provide bdecode)

(define (bdecode port)
  (let* ([c (integer->char (peek-byte port))])
    (cond
      [(char=? c #\i) (bdecode_int port)]
      [(char=? c #\d) (bdecode_dic port)]
      [(char=? c #\l) (bdecode_lst port)]
      [(char-numeric? c) (bdecode_str port)]
      [else (raise "Bdecoder - invalid formatting")]
      )))

(define (bdecode_int s)
  ; skips 'i'
  (read-byte s)
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
  (read-byte s)
  (let ([res (for/list ([c (in-port peek-char s)]
                        #:break (char=? c #\e))
               (bdecode s))])
    (read-byte s)
    res))

(define (bdecode_dic s)
  ; skips 'd'
  (read-byte s)
  (let ([res (for/hash ([c (in-port peek-char s)]
                        #:break (char=? c #\e))
               (values (bdecode_str s) (bdecode s)))])
    (read-byte s)
    res))
