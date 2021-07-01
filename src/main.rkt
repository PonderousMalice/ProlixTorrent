#lang racket

(require "tracker.rkt")

; some issues with multi files...
(define test_files '("C:\\Users\\david\\Downloads\\Lady_Confesses_archive.torrent"
                     "C:\\Users\\david\\Downloads\\[pornolab.net].t415569.torrent" ; marche po
                     "C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent"
                     "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent"))

(define juif (start_torrent (list-ref test_files 2)))
;(define test (parse_file (car test_files)))



;(define pute (extract_info_hash (car test_files)))
; ca40da6e934a70ccfa4a3f25e393a59a76ff8cd4