#lang racket

(require "util.rkt")
(require file/sha1)

(provide extract_info_hash)


; info-hash
(define (extract_info_hash path)
  (define in (open-input-file path #:mode 'binary))
  (displayln (skip_to_info in))
  (define counter 1)
  (define pipo #t)

  (define info (bytes-append* (for/list ([c (in-port read-byte in)]
                                         #:break (= 0 counter))
                                (cond
                                  [(or (= c (char->integer #\d)) (= c (char->integer #\l))) (if pipo (set! pipo #f) (set! counter (add1 counter))) (bytes c)]
                                  [(= c (char->integer #\e)) (set! counter (sub1 counter)) (bytes c)]
                                  [(= c (char->integer #\i))  (bytes-append #"i" (list->bytes (for/list ([c (in-port read-byte in)]
                                                                                                         #:break (= c (char->integer #\e)))
                                                                                                c)) #"e")]
                                  [else (let* ([str_l (list->bytes (cons c (for/list ([d (in-port read-byte in)]
                                                                                      #:break (= d (char->integer #\:)))
                                                                             d)))]
                                               ;[void (displayln str_l)]
                                               [length (stoi (bytes->string/utf-8 str_l))])
                                           
                                          (bytes-append str_l #":" (read-bytes length in)))]))))
  (close-input-port in)
  (bytes->hex-string (sha1-bytes info)))

	  
(define (skip_to_info in)
  (for ([c in]
        #:break (= c (char->integer #\4)))
    c)
  (displayln (peek-bytes 5 0 in))
  (if (bytes=? (peek-bytes 5 0 in) #":info")
      (read-bytes 5 in)
      (skip_to_info in)))