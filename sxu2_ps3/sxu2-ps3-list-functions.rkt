#lang racket
(provide (all-defined-out))


(define (map-remainder divisor ints)
  (if (null? ints)
      '()
      (cons (remainder (first ints) divisor) (map-remainder divisor (rest ints)))))

(define divisible-by?
  (lambda (num divisor)
    (= (remainder num divisor) 0)))

(define (filter-divisible-by divisor ints)
  (if (null? ints)
      '()
      (if (divisible-by? (first ints) divisor)
      (cons (first ints) (filter-divisible-by divisor (rest ints)))
      (filter-divisible-by divisor (rest ints)))))

(define (contains-multiple? m ns)
  (if (null? ns)
      #f
      (if (divisible-by? (first ns) m)
          #t
          (contains-multiple? m (rest ns)))))

(define (all-contain-multiple? n nss)
  (if (null? nss)
      #t
      (if (contains-multiple? n (first nss))
          (all-contain-multiple? n (rest nss))
          #f)))

(define (map-cons x ys)
  (if (null? ys)
      '()
      (cons (cons x (first ys)) (map-cons x (rest ys)))))

(define (my-cartesian-product xs ys)
  (if (null? xs)
      '()
      (append (map-cons (first xs) ys) (my-cartesian-product (rest xs) ys))))

(define (alts xs)
  (if (null? xs)
      '(() ())
      (let {[subres (alts (rest xs))]}
        (list (cons (first xs) (second subres)) (first subres)))))

(define (inserts x ys)
  (if (null? ys)
      (list (list x))
      (cons (cons x ys)
            (map-cons (first ys) (inserts x (rest ys))))))

(define (my-permutations xs)
  (if (null? xs)
      '(())
       (append-map (λ (ls) (inserts (first xs) ls)) (my-permutations (rest xs)))))
  