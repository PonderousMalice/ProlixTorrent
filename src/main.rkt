#lang racket

(require "bdecoder.rkt")
(require "bencoder.rkt")
(require simple-http)
(require openssl)
(require racket/random)

;(define multi_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent")))
(define single_file (bdecode (open-input-file "C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent")))


(let* ([info (dict-ref single_file #"info")]
  [info-hash (sha1-bytes (bencode info))]
  [peer_id (string-append "-PT1000-" (list->string (random-sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 12)))]
  ; Ports reserved for BitTirrent are typically 6881-6889
  [port 6881]
  [uploaded 0]
  [downloaded 0]
  [left (dict-ref info #"length")]
  [compact 1]
  [event "started"])


 info-hash
  )
 