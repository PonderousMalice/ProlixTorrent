#lang racket

(provide bencode)

; it's buggy on multi-files torrent
; but I don't use it so who cares lol

(define (bencode s)
  (cond
    [(number? s) (bencode_int s)]
  ;  [(and (dict? s) (bytes? (car s))) (bencode_dic s)]
    ;   [(and (pair? s) (bytes? (car s))) (bencode_pair s)]
    [(list? s) (bencode_lst s)]
    [(bytes? s) (bencode_str s)]
    [(hash? s) (bencode_dic s)]
 
    [else (error "Bencoder - unknown type")]))
  
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

(define (bencode_pair p)
  (bytes-append #"d" (bencode_str (car p)) (bencode (cdr p)) #"e"))