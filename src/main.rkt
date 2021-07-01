#lang racket

(require "bdecoder.rkt")
(require "bencoder.rkt")

(require net/http-client)
(require net/url)
(require net/uri-codec)
(require file/sha1)
(require openssl)
(require racket/random)

(define (tracker_get announce_url info_hash peer_id uploaded downloaded left compact event)
  ;url encode hash-info
  (define url_encoded_info_hash (string-append* (for/list ([c info_hash]
                                                           [i (in-range (string-length info_hash))])
                                                  (if (odd? i) (string c) (string #\% c)))))
  
  (http-sendrecv (url-host announce_url)
                 (string-append "/" (path/param-path (car (url-path announce_url)))
                                "?info_hash=" url_encoded_info_hash
                                "&peer_id="  peer_id
                                "&uploaded=" (~v uploaded)
                                "&downloaded=" (~v downloaded)
                                "&left=" (~v left)
                                "&compact=" (if compact "1" "0")
                                "&event=" event)
                 #:ssl? (string=? "https" (url-scheme announce_url))
                 #:version "1.1"
                 #:method "GET"
                 #:port (url-port announce_url)))

;(define multi_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent")))
(define single_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent")))

; Ports reserved for BitTirrent are typically 6881-6889
(define port 6881)

(define info (dict-ref single_file #"info"))
(define announce_url (string->url (bytes->string/utf-8 (dict-ref single_file #"announce"))))
(define info_hash (bytes->hex-string (sha1-bytes (bencode info))))
(define peer_id (string-append "-PT1000-" (list->string (random-sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 12))))
(define uploaded 0)
(define downloaded 0)
(define left (dict-ref info #"length"))
(define compact #t)
(define event "started")

;  [left (~v (dict-ref info #"length"))]

      
(define-values (status headers in)
  (tracker_get announce_url info_hash peer_id uploaded downloaded left compact event))

(displayln status)
(displayln headers)
(displayln (port->string in))
(close-input-port in)                


  
  
