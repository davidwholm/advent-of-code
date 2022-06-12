#lang racket

(define (compute-offset direction)
  (match direction
    [#\> (cons 1 0)]
    [#\< (cons -1 0)]
    [#\^ (cons 0 1)]
    [#\v (cons 0 -1)]))

(define/match (apply-offset location offset)
  [((cons x y)
    (cons dx dy))
   (cons (+ x dx)
         (+ y dy))])

(define (directions->visits directions)
  (for/fold ([visits (list (cons 0 0))])
            ([direction directions])
    (append visits (list (apply-offset (last visits) (compute-offset direction))))))

(module+ test
  (require rackunit)
  (check-equal? (directions->visits '(#\>))
                (list (cons 0 0)
                      (cons 1 0)))
  (check-equal? (directions->visits '(#\^ #\> #\v #\<))
                (list (cons 0 0)
                      (cons 0 1)
                      (cons 1 1)
                      (cons 1 0)
                      (cons 0 0)))
  (check-equal? (directions->visits '(#\^ #\v #\^ #\v #\^ #\v #\^ #\v #\^ #\v))
                (list (cons 0 0)
                      (cons 0 1)
                      (cons 0 0)
                      (cons 0 1)
                      (cons 0 0)
                      (cons 0 1)
                      (cons 0 0)
                      (cons 0 1)
                      (cons 0 0)
                      (cons 0 1)
                      (cons 0 0))))
                
(define (part-1 filename)
  (length (remove-duplicates (directions->visits (string->list (file->string filename))))))

(define (directions->visits/robo-santa directions)
  (for/fold ([santa-visits (list (cons 0 0))]
             [robo-visits (list (cons 0 0))])
            ([direction directions])
    (if (<= (length santa-visits)
            (length robo-visits))
        (values (append santa-visits (list (apply-offset (last santa-visits) (compute-offset direction))))
                robo-visits)
        (values santa-visits
                (append robo-visits (list (apply-offset (last robo-visits) (compute-offset direction))))))))

(define (part-2 filename)
  (let-values ([(santa-visits robo-visits) (directions->visits/robo-santa (string->list (file->string filename)))])
    (length (remove-duplicates (append santa-visits robo-visits)))))