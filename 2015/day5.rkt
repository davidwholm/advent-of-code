#lang racket

(define (contains-substrings?/any-of str #:substrings substrings)
  (for/or ([substr substrings])
    (string-contains? str substr)))

(define (contains-chars?/pred str #:satisfying pred #:minimum-length [minimum-length 1])
  (let ([found-chars (filter pred (string->list str))])
    (>= (length found-chars)
        minimum-length)))
  
(define (vowel? ch)
  (member ch (list #\a #\e #\i #\o #\u) char=?))

(define (contains-consecutive-elements? lst #:minimum-length minimum-length)
  (if (or (empty? lst)
          (< (length lst) minimum-length))
      #f
      (or (equal? (take lst minimum-length)
                  (make-list minimum-length (first lst)))
          (contains-consecutive-elements? (rest lst) #:minimum-length minimum-length))))

(define (nice-string? str)
  (and (not (contains-substrings?/any-of str #:substrings (list "ab" "cd" "pq" "xy")))
       (contains-chars?/pred str #:satisfying vowel? #:minimum-length 3)
       (contains-consecutive-elements? (string->list str) #:minimum-length 2)))

(define (classify str)
  (if (nice-string? str)
      'nice
      'naughty))

(define (part-1 filename)
  (length (filter (λ (c)
                    (eq? c 'nice))
                  (map classify (file->lines filename)))))

(define (nice-string?/v2 str)
  (and (contains-repeating-substr? str #:substr-length 2)
       (contains-repeating-char?/gap str #:gap 1)))

(define (classify/v2 str)
  (if (nice-string?/v2 str)
      'nice
      'naughty))

(define (part-2 filename)
  (length (filter (λ (c)
                    (eq? c 'nice))
                  (map classify/v2 (file->lines filename)))))