#lang racket
(provide (all-defined-out))

;; takes two arguments and returns an integer list the same length as ints
;; in which every element is remainder of dividing the corresponding element of ints by divisor
(define (map-remainder divisor ints)
  (map (λ (int) (remainder int divisor)) ints))

;; takes two arguments - an integer and a list of ints
;; returns a new integer list containing all the elements of ints that are divisible by divisor
(define (filter-divisible-by divisor ints)
  (filter (λ (int) (divisible-by? int divisor)) ints))

(define divisible-by?
  (lambda (num divisor)
    (= (remainder num divisor) 0)))

;; takes an integer and a list of integers
;; returns #t if m evenly divides at least one elt of the integer list
(define (contains-multiple? m ns)
  (foldr (λ (n l) (or (divisible-by? n m) l)) #f ns))

;; takes an integer and a list of lists of integers
;; returns #t if each list of integers in nss contains at least one integer
;; that's a multiple of n
(define (all-contain-multiple? n nss)
  (foldr (λ (ns l) (and (contains-multiple? n ns) l)) #t nss))

;; takes a value and a list
;; returns the new list that results from adding x to end of ys
(define (snoc x ys)
  (foldr cons (list x) ys))

;; takes two lists - xs and ys
;; returns the new list that contains all elts of xs followed by elts of ys
(define (my-append xs ys)
  (foldr cons ys xs))

;; takes a list of lists xss
;; returns a new list that contains all elts of the sublists of xss in relative order
(define (append-all xss)
  (foldr append '() xss))

;; takes any value x and n-elt list ys
;; returns an n-elt list of all pairs '(x.y)
(define (map-cons x ys)
  (map (λ (y) (cons x y)) ys))

;; takes two lists - xs and ys
;; returns a list of all pairs '(x.y)
(define (my-cartesian-product xs ys)
  (foldr (λ (x l) (append (map-cons x ys) l)) '() xs))

;; takes a list xs
;; returns a new list whose elts are elts of xs in reverse order
(define (my-reverse xs)
  (foldr (λ (x l) (snoc x l)) '() xs))

;; takes a list
;; returns a two-element list of lists
;; first has all the even-index elts, the second has all odd-indexed elts
(define (alts xs)
  (foldr (λ (x l) (list (cons x (second l)) (first l))) '(() ()) xs))

;; takes a value x and an n-elt list ys
;; returns an n+1-elt list of lists showing all ways to insert x into ys
(define (inserts-foldr x ys)
  (foldr (λ (y l) (cons (append (list x y) (rest (first l))) (map-cons y l)))
         (list (list x)) ys))

;; takes a list xs of distinct elements
;; returns a list of all the permutations of the elts of xs
(define (my-permutations xs)
  (foldr (λ (x l) (append-all (map (λ (m) (inserts-foldr x m)) l))) '(()) xs))

(define (forall? pred xs)
  (if (null? xs)
      #t
      (and (pred (car xs))
           (forall? pred (cdr xs)))))

(define (exists? pred xs)
  (if (null? xs)
      #f
      (or (pred (car xs))
          (exists? pred (cdr xs)))))

(define (find pred not-found xs)
  (if (null? xs)
      not-found
      (if (pred (car xs))
          (car xs)
          (find pred not-found (cdr xs)))))

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      null
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;; determines if an elt x appears in list ys
;; returns #t if so, #f if not
(define (member? x ys)
  (exists? (λ (y) (equal? x y)) ys))

;; alternative implementation of all-contain-multiple?
(define (all-contain-multiple-alt? m nss)
  (forall? (λ (ns) (exists? (λ (n) (divisible-by? n m)) ns)) nss))

;; takes a key k and an association list as
;; returns #f is no mapping with k was found; and otherwise
;; a cons cells whose car is k and whose cdr is corresponding value
;; for the shallowest mapping of k
(define (lookup k as)
  (find (λ (a) (equal? k (car a))) #f as))

;; determines if a list of nums is in sorted order from low to high
(define (sorted? ns)
  (if (null? ns)
      #t
      (forall? (λ (p) (<= (car p) (cdr p))) (zip ns (rest ns)))))

(define (foldr-ternop ternop null-value xs)
  (if (null? xs)
      null-value
      (ternop (first xs)
              (rest xs)
              (foldr-ternop ternop null-value (rest xs)))))

;; takes a value x and an n-elt list ys
;; returns an n+1 elt list of lists showing all ways to insert x into ys
(define (inserts-foldr-ternop x ys)
  (foldr-ternop (λ (fy ry l) (cons (cons x (cons fy ry)) (map-cons fy l)))
                (list (list x)) ys))

;; alternative implementation of the sorted function
(define (sorted-alt? xs)
  (foldr-ternop (λ (f r l) (and (<= f (if (null? r)
                                          +inf.0
                                          (first r))) l))
                #t xs))