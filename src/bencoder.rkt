#lang racket

(provide bencode)

(define (bencode s)
  (cond
    [(string? s) (bencode_str s)]
    [(number? s) (bencode_int s)]
    [(dict? s) (bencode_dic s)]
    [(list? s) (bencode_lst s)]
    [(bytes? s) (bencode_str s)]
    [else "unknown type"]))
  
(define (bencode_str str)
  (bytes-append (string->bytes/utf-8 (~v (bytes-length str))) #":" str))

(define (bencode_int i)
  (bytes-append #"i" (string->bytes/utf-8 (~v i)) #"e"))

(define (bencode_lst l)
  (bytes-append
   (foldl (lambda (e res)
            (bytes-append res (bencode e))) #"l" l)
   #"e"))

(define (bencode_dic d)
  (bytes-append (foldl (lambda (e res)
                         (bytes-append res (bencode_str (car e)) (bencode (cdr e))))
                       #"d" (dict->list d))
                #"e"))