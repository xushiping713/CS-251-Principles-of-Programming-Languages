#lang racket
(provide (all-defined-out))

(define sum-squares-of-ints-divisible-by
  (lambda (divisor lo hi)
    (if (> lo hi)
        0
        (if (divisible-by? lo divisor)
            (+ (* lo lo) (sum-squares-of-ints-divisible-by divisor (+ lo 1) hi))
            (+ 0 (sum-squares-of-ints-divisible-by divisor (+ lo 1) hi))))))

(define divisible-by?
  (lambda (num divisor)
    (= (remainder num divisor) 0)))


(define hamming?
  (lambda (num)
    (and (integer? num) (< 0 num)
         (or (= 1 num)
             (and (divisible-by? num 2) (hamming? (/ num 2)))
             (and (divisible-by? num 3) (hamming? (/ num 3)))
             (and (divisible-by? num 5) (hamming? (/ num 5)))))))

