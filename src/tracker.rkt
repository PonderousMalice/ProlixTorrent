#lang racket

(provide start_torrent)
(provide parse_file)

(require racket/random)
(require net/url)
(require net/http-client)
(require file/sha1)
(require racket/tcp)

(require "bdecoder.rkt")
(require "bencoder.rkt")
(require "torrent.rkt")
(require "util.rkt")

(struct torrent (announce_url
                 info_hash
                 uploaded
                 downloaded
                 left))

(struct peer (ip
              port
              am_choking
              am_interested
              peer_choking
              peer_interested))

(struct connection (peer
                    torrent
                    in
                    out))

(define event '("started" "stopped" "completed"))

; options
(define peer_id (string-append "-PT1000-" (list->string (random-sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 12))))
(define compact #t)

(define (parse_file path)
  (bdecode (open-input-file path)))

(define (compute_left info)
  (if (hash-has-key? info #"files")
      (for/sum ([f (dict-ref info #"files")])
         (dict-ref f #"length"))
      (dict-ref info #"length")))

(define (init_torrent dic i_hash)
  (let* ([info (dict-ref dic #"info")]
         [announce_url (bytes->string/utf-8 (dict-ref dic #"announce"))]
         [info_hash i_hash]
         [left (compute_left info)]
         )
    (torrent announce_url info_hash 0 0 left)))

(define (contact_tracker t)
  (define url_encoded_info_hash (string-append* (for/list ([c (torrent-info_hash t)]
                                                           [i (in-range (string-length (torrent-info_hash t)))])
                                                  (if (odd? i) (string c) (string #\% c)))))
  (define url (string->url (torrent-announce_url t)))
  (if (boolean? (url-port url)) (error "Invalid port")
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
                     #:port (url-port url))))

(define (convert_port bytes)
  (+ (arithmetic-shift (bytes-ref bytes 0) 8) (bytes-ref bytes 1)))

; The compact response is 6 bytes per peer.
; The first 4 bytes are the host
; the last 2 bytes are the port
; Both in network byte order
(define (convert_peers peers)
  (displayln (bytes->hex-string peers))
  (for/list ([b (bytes-split peers 6)])
    (let ([ip (foldl (lambda (e res)
                       (string-append res (~v e) ".")) "" (bytes->list (subbytes b 0 4)))]
          [port (convert_port (subbytes b 4))])
      
      (peer (substring ip 0 (- (string-length ip) 1)) port 1 0 1 0))))

(define (start_torrent path)
  (define dic (parse_file path))
  (define info_hash (extract_info_hash path))
  (displayln info_hash)
  (define t (init_torrent dic info_hash))
  (define-values (status headers in)
    (contact_tracker t))

  (displayln status)
  (displayln headers)
  (define response (bdecode in))
  (define pd (dict-ref response #"peers"))
  (define peers (convert_peers pd))
  (displayln (length peers))
  (for ([p peers])
    (displayln (peer-ip p))
    (displayln (peer-port p)))
  

  ; (displayln "Attemping to connect...")
  ; (peer_connect (list-ref peers 0) t)
  ; (displayln "Attemping to connect...2")
  ;(peer_connect (list-ref peers 1) t)

  (close-input-port in))


;for/list getting all connections
(define (handshake_msg p t)
  (let ([pstr #"BitTorrent protocol"]
        [info_hash (string->bytes/utf-8 (torrent-info_hash t))]
        [reserved (make-bytes 8 0)] ; for bittorrent extensions
        [p_id (string->bytes/utf-8 peer_id)])
    (bytes-append (bytes (bytes-length pstr)) pstr reserved info_hash p_id)))

(define (peer_connect p t)
  (define-values (in out)
    (tcp-connect (peer-ip p) (peer-port p)))
  (write (handshake_msg p t) out)
  (flush-output out) ; forces the data to be physically written
  (displayln (read in))

  (close-input-port in)
  (close-output-port out))
  
