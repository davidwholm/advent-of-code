#lang racket

(require file/md5
         racket/trace)

(define (hash-with-leading-zeros secret [num 1] #:leading-zeros [zeros 5])
  (define md5-hash (md5 (string-append secret (number->string num)) #t))
  (if (and (>= (bytes-length md5-hash) zeros)
           (bytes=? (subbytes md5-hash 0 zeros)
                    (make-bytes zeros (char->integer #\0))))
      num
      (hash-with-leading-zeros secret (add1 num) #:leading-zeros zeros)))

(define (part-1 filename)
  (hash-with-leading-zeros (file->string filename)))

(define (part-2 filename)
  (hash-with-leading-zeros (file->string filename) #:leading-zeros 6))