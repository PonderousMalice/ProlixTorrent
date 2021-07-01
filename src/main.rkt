#lang racket

(require "tracker.rkt")


(define test_files '("C:\\Users\\david\\Downloads\\debian-10.5.0-amd64-DVD-1.iso.torrent"
                     "C:\\Users\\david\\Downloads\\Le président des ultra-riches - Michel Pinçon & Monique Pinçon-Charlot.torrent"))

(start_torrent (car test_files))



  
  
