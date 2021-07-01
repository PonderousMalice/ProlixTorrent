#lang racket

(define (bencode s)
  (cond
    [(string? s) (bencode_str s)]
    [(number? s) (bencode_int s)]
    [(list? s) (bencode_lst s)]
    [(dict? s) (bencode_dic s)]
    [else " "]))
  
(define (bencode_str str)
  (string-append (~v (string-length str)) ":" str))

(define (bencode_int i)
  (string-append "i" (~v i) "e"))

(define (bencode_lst l)
  (string-append
   (foldl (lambda (e res)
           (string-append res (bencode e))) "l" l)
  "e"))

(define (bencode_dic d)
  (string-append (foldl (lambda (e res)
                          (string-append res (bencode_str (car e)) (bencode (cdr e))))
                        "d" (dict->list d))
                 "e"))
                   

