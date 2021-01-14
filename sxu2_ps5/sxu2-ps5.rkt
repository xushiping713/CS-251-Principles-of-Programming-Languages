#lang racket
(provide (all-defined-out))

;; given a starting number n
;; returns a list of duples for the Collatz sequence starting with n
;; in which each duple has (1) the current step number
;; (2) the element of the Collatz sequence for the current step
(define (collatz-genlist-apply num)
  (genlist-apply (λ (i n) (list (+ i 1)
                                (if (even? n)
                                    (quotient n 2)
                                    (+ 1 (* 3 n))))) ; next
                 (λ (i n) (= 1 n)); done?
                 #t ; keepDoneValue?
                 (list 0 num); seed
                 ))

(define (genlist-apply next done? keepDoneValue? seed)
  (if (apply done? seed)
      (if keepDoneValue? (list seed) null)
      (cons seed
            (genlist-apply next done? keepDoneValue? (apply next seed)))))

;; same input-output behavior as collatz-genlist-apply
;; but definition uses iterate-apply
(define (collatz-iterate-apply num)
  (iterate-apply (λ (step num pairsSoFar)
                   (list (+ step 1)
                         (if (even? num) (quotient num 2) (+ 1 (* 3 num)))
                         (cons (list step num) pairsSoFar))) ; next
                 (λ (step num pairsSoFar) (= 1 num)); done?
                 (λ (step num pairsSoFar) (reverse (cons (list step num) pairsSoFar))); finalize 
                 (list 0 num '()) ; initial state
                 ))
                  
(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

;; evaluating a polynomial using Horner's method using foldl
(define (poly-eval coeffs x)
  (foldl (λ (coeff rlt) (+ coeff (* x rlt)))
         0 
         coeffs))

;; takes a nonnegative integer n
;; returns a list of bits for the binary representation of n
(define (bits num)
  (iterate-apply (λ (n l) (list (quotient n 2) (cons (remainder n 2) l))) ; next
                 (λ (n l) (= 0 n)); done?
                 (λ (n l) (if (= 0 num) '(0) l)); finalize
                 (list num '()); initial state
                 ))

;; has the same input-output behavior as pairs function
;; takes a single argument n, which is a positive integer
;; returns a list of pairs in terms of n
(define (pairs-hof n)
  (foldr (λ (diff l)
           (append (map (λ (str) (list str (+ str diff)))
                        (range 0 (- (+ 1 n) diff)))
                   l))
         '() ; null value
         (range 1 (+ 1 n))))

;; has the same input-output behavior as pairs function
;; takes a single argument n, which is a positive integer
;; returns a list of pairs in terms of n
(define (pairs-genlist-apply n)
  (genlist-apply (λ (st stdiff)
                   (if (= stdiff n)
                       (list 0 (+ 1 (- stdiff st)))
                       (list (+ 1 st) (+ 1 stdiff)))) ; next
                 (λ (st stdiff) (= n (- stdiff st))) ; done?
                 #t; keepDoneValue?
                 (list 0 1); seed
                 ))
   
;; has the same input-output behavior as pairs function
;; takes a single argument n, which is a positive integer
;; returns a list of pairs in terms of n
(define (pairs-iterate-apply n)
  (iterate-apply (λ (diff start pairsSoFar)
                   (if (= start 0)
                       (list (- diff 1)
                             (- (+ 1 n) diff)
                             (cons (list start (+ start diff)) pairsSoFar))
                       (list diff
                             (- start 1)
                             (cons (list start (+ start diff)) pairsSoFar)))
                   ) ; next
                 (λ (diff start pairsSoFar)
                   (and (= 1 diff) (= 0 start))); done?
                 (λ (diff start pairsSoFar)
                   (cons (list start diff) pairsSoFar)); finalize
                 (list n 0 '()); initial state
                 ))

;; has the same input-output behavior as pairs function
;; but is implemented by calling a pairs-tail function
(define (pairs-iter num)
  (pairs-tail num num 0 '()))

(define (pairs-tail n diff start pairsSoFar)
  (if (and (= diff 1) (= start 0))
      (cons (list start diff) pairsSoFar)
      (let ([pairs (cons (list start (+ start diff)) pairsSoFar)])
      (if (= start 0)
          (pairs-tail n
                      (- diff 1)
                      (- (+ n 1) diff)
                      pairs)
          (pairs-tail n
                      diff
                      (- start 1)
                      pairs))))
  )

;; has the same input-output behavior as pairs function
;; but is implemented by calling pairs-outer-tail and pairs-inner-tail
(define (pairs-iter-nested n)
  (define (pairs-outer-tail diff pairsSoFar)
    (define (pairs-inner-tail start pairsSoFar-inner) 
          (if (= start 0)
              (pairs-outer-tail (- diff 1) (cons (list start (+ start diff)) pairsSoFar-inner))
              (pairs-inner-tail (- start 1)
                                (cons (list start (+ start diff)) pairsSoFar-inner)))
      )
    (if (= diff 0)
        pairsSoFar
        (pairs-inner-tail (- n diff) pairsSoFar))
    )
  (pairs-outer-tail n '()))


(define (process ints)
  (add-mode 0 ints))

(define (add-mode ans ints)
  (if (null? ints)
      ans
      (let ((fst (first ints))
            (rst (rest ints)))
        (cond [(< fst 0) (subtract-mode fst ans rst)]
              [(= fst 0) (skip-mode-start ans rst)]
              [(= fst 42) (add-mode ans null)]
              [else (add-mode (+ fst ans) rst)]))))

 (define (subtract-mode end ans ints)
   (if (null? ints)
       ans
       (let ((fst (first ints))
             (rst (rest ints)))
         (cond [(= fst end) (add-mode ans rst)]
               [else (subtract-mode end (- ans fst) rst)]))))

 (define (skip-mode-start ans ints)
    (if (null? ints)
        ans
        (skip-mode (abs (first ints)) ans (rest ints))))

 (define (skip-mode number-of-elements-to-skip ans ints)
   (if (null? ints)
       ans
       (let ((fst (first ints))
             (rst (rest ints)))
         (cond [(> number-of-elements-to-skip 0) (skip-mode (- number-of-elements-to-skip 1) ans rst)]
               [(= number-of-elements-to-skip 0) (add-mode (+ fst ans) rst)]))))
   
(define (test-case expected nums)
  (let ((ans (process nums)))
    (if (= ans expected)
        (display (string-append "process passed test with answer " (number->string expected) "\n"))
        (display (string-append "*** ERROR: process got"
                                (number->string ans)
                                "but expected"
                                (number->string expected)
                                "\n")))))

(define (test-all)
    (test-case 15 '(1 2 3 4 5))
    (test-case 19 '(1 7 2 9))
    (test-case 15 '(1 2 3 4 5 42 6 7))
    (test-case 6 '(1 2 3 42 4 5 42 6 7))
    (test-case 0 '(42 1 2 3 4 5 6 7))
    (test-case 10 '(1 2 3 -17 4 5 -17 6 7))
    (test-case 20 '(1 2 -1 4 6 -1 7 8 -5 9 -5 10 11))
    (test-case 7 '(1 2 3 -1 4 -5 6 -1 7 8 -5 9))
    (test-case -35 '(1 2 -1 4 42 5 -1 6 7))
    (test-case 26 '(4 5 0 2 6 7 8 9))
    (test-case 14 '(7 2 0 3 6 1 8 5 0 4 9 10))
    (test-case 10 '(7 3 0))
    (test-case 2 '(7 3 0 4 -1 0 8 42 5 -1 4 9))
    (test-case 499999499097 (range 43  1000000))
    (test-case 7999997999097 (range 43  4000000))
    (test-case -42 (append (range 43  1000000) '(-17) (range 0 1000000) '(-17) (range 1 43)))
    (test-case -903 (append (range 43  1000000) '(-17) (range 0 1000000) '(-17  42) (range 1 43)))
    (test-case -581 (append (range 43  1000000) '(-17) (range 0 1000000) '(-17  0  42) (range 1 50)))
  )