#lang racket

(require rackunit)

(define (calculate-floor instrs)
  (for/fold ([floor 0]) ([instr instrs])
    (match instr
      [#\( (add1 floor)]
      [#\) (sub1 floor)])))

(define (part-1 filename)
  (calculate-floor (file->string filename)))

(define (basement-position instrs)
  (let ([floor-seq (for/fold ([floor-seq '(0)])
                             ([instr instrs])
                     (append floor-seq
                             (list ((match instr
                                      [#\( add1]
                                      [#\) sub1]) (last floor-seq)))))])
    (or (index-of floor-seq -1)
        #f)))

(define (part-2 filename)
  (basement-position (file->string filename)))

(module+ test
  (require rackunit)
  ;; Part 1
  (check-equal? (calculate-floor "(())") 0)
  (check-equal? (calculate-floor "()()") 0)
  (check-equal? (calculate-floor "(((") 3)
  (check-equal? (calculate-floor "(()(()(") 3)
  ;; Part 2
  (check-equal? (basement-position ")") 1)
  (check-equal? (basement-position "()())") 5))
