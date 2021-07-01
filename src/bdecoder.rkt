#lang racket

(define (bdecode s)
  (let* ([str (open-input-string s)]
         [c (peek-char str)])
    (cond
      [(char=? c #\i) (bdecode_int str)]
   ;   [(char=? c #\d) (bdecode_dic s)]
   ;   [(char=? c #\l) (bdecode_lst s)]
      [(char-numeric? c) (bdecode_str str)]
     )

   ; (display (read-char str))
             ))

(define (bdecode_int s)
  ; skips 'i'
  (read-char s)
  (stoi (list->string (for/list ([c (in-input-port-chars s)]
             #:break (char=? c #\e))
    c))))

(define (bdecode_str s)
  (let ([length (stoi (list->string (for/list ([c (in-input-port-chars s)]
                                               #:break (char=? c #\:))
                                      c)))])
    (read-string length s)))


; utilitary functions
(define (ctoi c)
  (cond
    [(char=? c #\0) 0]
    [(char=? c #\1) 1]
    [(char=? c #\2) 2]
    [(char=? c #\3) 3]
    [(char=? c #\4) 4]
    [(char=? c #\5) 5]
    [(char=? c #\6) 6]
    [(char=? c #\7) 7]
    [(char=? c #\8) 8]
    [(char=? c #\9) 9]))

(define (stoi str)
  (let ([len (string-length str)])
    (cond
    [(equal? len 0) 0]
    [(equal? len 1) (ctoi (string-ref str 0))]
    [else (+ (ctoi (string-ref str (- len 1))) (* 10 (stoi (substring str 0 (- len 1)))))])))