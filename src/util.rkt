#lang racket

(provide stoi)
(provide bytes-split)

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

; transforms a bytes-string into a list of n bytes
(define (bytes-split b n)
  (cond
    [(= 0 n) '()]
    [(>= n (bytes-length b)) (cons b null)]
    [else (cons (subbytes b 0 n) (bytes-split (subbytes b n (bytes-length b)) n))]))

