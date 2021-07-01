#lang racket

(require "bdecoder.rkt")
(require "bencoder.rkt")

(require net/http-client)
(require net/url)
(require net/uri-codec)
(require file/sha1)
(require openssl)
(require racket/random)

;(define multi_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent")))
(define single_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent")))


(let* ([announce_url (string->url (bytes->string/utf-8 (dict-ref single_file #"announce")))]
  [info (dict-ref single_file #"info")]
  [info-hash (bytes->hex-string (sha1-bytes (bencode info)))]
  [peer_id (string-append "-PT1000-" (list->string (random-sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 12)))]
  ; Ports reserved for BitTirrent are typically 6881-6889
  [port 6881]
  [uploaded 0]
  [downloaded 0]
  [left (~v (dict-ref info #"length"))]
  [compact 1]
  [event "started"])
  
  (define url_encoded_info_hash (string-append* (for/list ([c info-hash]
             [i (in-range (string-length info-hash))])
    (if (odd? i) (string c) (string #\% c)))))
    
  
  (define-values (status headers in)
    (http-sendrecv (url-host announce_url)
                   (string-append "/" (path/param-path (car (url-path announce_url))) "?info_hash=" url_encoded_info_hash "&peer_id="  peer_id "&uploaded=0&downloaded=0&left=" left "compact=1&event=started")
                   #:ssl? #f;(string=? "https" (url-scheme announce_url))
                   #:version "1.1"
                   #:method "GET"
                   #:port (url-port announce_url)))

  (displayln status)
  (displayln headers)
  (displayln (port->string in))
  (close-input-port in)
  
                   
  )


 