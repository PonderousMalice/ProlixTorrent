#lang racket

(provide start_torrent)

(require racket/random)
(require net/url)
(require net/http-client)
(require file/sha1)
;(require openssl)

(require "bdecoder.rkt")
(require "bencoder.rkt")

(struct torrent (announce_url
                 info_hash
                 uploaded
                 downloaded
                 left))

(define test_file "C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent")

(define event '("started" "stopped" "completed"))

; options
(define peer_id (string-append "-PT1000-" (list->string (random-sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 12))))
(define compact #t)

(define (parse_file path)
  (bdecode (open-input-file path)))

(define (init_torrent dic)
  (let* ([info (dict-ref dic #"info")]
         [announce_url (bytes->string/utf-8 (dict-ref dic #"announce"))]
         [info_hash (bytes->hex-string (sha1-bytes (bencode info)))]
         [left (dict-ref info #"length")])
    (torrent announce_url info_hash 0 0 left)))

(define (contact_tracker t)
  (define url_encoded_info_hash (string-append* (for/list ([c (torrent-info_hash t)]
                                                           [i (in-range (string-length (torrent-info_hash t)))])
                                                  (if (odd? i) (string c) (string #\% c)))))
  (define url (string->url (torrent-announce_url t)))
  (http-sendrecv (url-host url)
                 (string-append "/" (path/param-path (car (url-path url)))
                                "?info_hash=" url_encoded_info_hash
                                "&peer_id="  peer_id
                                "&uploaded=" (~v (torrent-uploaded t))
                                "&downloaded=" (~v (torrent-downloaded t))
                                "&left=" (~v (torrent-left t))
                                "&compact=" (if compact "1" "0")
                                "&event=" (car event))
                 #:ssl? (string=? "https" (url-scheme url))
                 #:version "1.1"
                 #:method "GET"
                 #:port (url-port url)))


(define (start_torrent path)
  (define-values (status headers in)
    (contact_tracker (init_torrent (parse_file path))))
  (displayln status)
  (displayln headers)
  (displayln (port->string in))
  (close-input-port in))