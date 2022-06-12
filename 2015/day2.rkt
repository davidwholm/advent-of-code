#lang racket

(struct Box
  (length width height)
  #:transparent)

(define (box-areas box)
  (match-let ([(Box l w h) box])
    (list (* l w)
          (* w h)
          (* h l))))

(define (surface-area box)
  (apply + (map (λ (a)
                  (* 2 a)) (box-areas box))))

(define (extra-paper box)
  (apply min (box-areas box)))

(define (wrapping-paper box)
  (+ (surface-area box)
     (extra-paper box)))

(define (string->Box str)
  (apply Box (map string->number (regexp-split #rx"x" str))))

(define (file->Boxes filename)
  (map string->Box (file->lines filename)))

(define (part-1 filename)
  (apply + (map wrapping-paper (file->Boxes filename))))

(define (box-perimeters box)
  (match-let ([(Box l w h) box])
    (map (match-lambda [(cons n m) (+ (* 2 n)
                                      (* 2 m))])
         (list (cons l w)
               (cons w h)
               (cons h l)))))

(define (smallest-perimeter box)
  (apply min (box-perimeters box)))

(define (cubic-volume box)
  (match-let ([(Box l w h) box])
    (* l w h)))

(define (wrapping-ribbon box)
  (+ (smallest-perimeter box)
     (cubic-volume box)))

(define (part-2 filename)
  (apply + (map wrapping-ribbon (file->Boxes filename))))

(module+ test
  (require rackunit)
  ;; Part 1
  (check-equal? (wrapping-paper (Box 2 3 4))
                58)
  (check-equal? (wrapping-paper (Box 1 1 10))
                43)
  ;; Part 2
  (check-equal? (wrapping-ribbon (Box 2 3 4))
                34)
  (check-equal? (wrapping-ribbon (Box 1 1 10))
                14))
