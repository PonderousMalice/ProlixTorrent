#lang racket

(require "tracker.rkt")

; some issues with multi files...
(define test_files '("C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent"
                     "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent"))

(define juif (start_torrent (car test_files)))

  
