#lang racket
(provide (all-defined-out))

;;;;**********************************************************************
;;; CS251 Spring 2017 PS6
;;; Shiping Xu
;;;;**********************************************************************

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Solo Problem 1: Diagonal Duples

;;;-----------------------------------------------------------------------
;; You are given the following function.
;; In Problem 1a, explain what it returns
(define (diagonal-duples n) ; Assume is n a nonnegative integer
  (foldr append null
         (map (λ (sum)
                (map (λ (fst) (list fst (- sum fst)))
                     (range 0 (+ sum 1))))
              (range 0 (+ n 1)))))

;;;-----------------------------------------------------------------------
;; Flesh out the following skeleton for Problem 1b
(define (diagonal-duples-genlist-apply n) ; Assume is n a nonnegative integer
  (genlist-apply (λ (i j)
                   (if (= 0 j)
                       (list 0 (+ i 1))
                       (list (+ i 1) (- j 1)))); next
                 (λ (i j) (and (= n i) (= 0 j))); done?
                 #t ; keepDoneValue?
                 (list 0 0) ; seed
                 ))

;;;-----------------------------------------------------------------------
;; Flesh out the following skeleton for Problem 1c
(define (diagonal-duples-iterate-apply n) ; Assume is n a nonnegative integer
  (iterate-apply (λ (sum fst pairsSoFar)
                   (if (= sum fst)
                       (list (- sum 1)
                             0
                             (cons (list 0 fst) pairsSoFar))
                       (list sum
                             (+ fst 1)
                             (cons (list (- sum fst) fst) pairsSoFar)))
                   ); next
                 (λ (sum fst pairsSoFar)
                   (and (= sum 0) (= fst 0))); done?
                 (λ (sum fst pairsSoFar)
                   (cons (list sum fst) pairsSoFar)); finalize
                 (list n 0 '()); initial state
                 ))

;;;-----------------------------------------------------------------------
;; Flesh out the following skeleton for Problem 1d
(define (diagonal-duples-iter n)
  (define (diagonal-duples-outer-tail sum pairsSoFar)
    (define (diagonal-duples-inner-tail fst pairsSoFar-inner)
      (if (= sum fst)
          (diagonal-duples-outer-tail (- sum 1)
                                      (cons (list 0 fst) pairsSoFar-inner))
          (diagonal-duples-inner-tail (+ fst 1)
                                      (cons (list (- sum fst) fst)
                                            pairsSoFar-inner)))
      )
    (if (= sum 0)
        (cons (list sum 0) pairsSoFar)
        (diagonal-duples-inner-tail 0 pairsSoFar))
      )
  (diagonal-duples-outer-tail n '()))

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Helper Functions from PS5 and iteration lectures

(define (genlist-apply next done? keepDoneValue? seed)
  (if (apply done? seed)
      (if keepDoneValue? (list seed) null)
      (cons seed
            (genlist-apply next done? keepDoneValue? (apply next seed)))))

(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

