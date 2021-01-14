#lang racket

;;;;**********************************************************************
;;; CS251 Spring 2017 PS7 Solo Problems 1 and 2
;;; Shiping Xu
;;;;**********************************************************************

;;;----------------------------------------------------------------------
;;; Solo Problem 1: It's A Factor

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; You are given these three functions

(define (least-divisor-rec num) ;; Assume num is a postive integer
  (let ((limit (ceiling (sqrt num)))) ;; The largest divisor to be tested
    (define (search-for-divisor candidate)
      (if (> candidate limit)
          num
          (if (divisible-by? num candidate)
              candidate
              (search-for-divisor (+ candidate 2))))) 
    (if (divisible-by? num 2)
        2
        (search-for-divisor 3)))) 

(define (divisible-by? num divisor)
  (= (remainder num divisor) 0))

(define (factors-rec num)
  (let ((factor (least-divisor-rec num)))
    (if (= factor num)
        (list factor)
        (cons factor (factors-rec (quotient num factor))))))
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Flesh this out for Problem 1a
(define (hamming? num)
  (and (integer? num)
       (> num 0)
       (or (= num 1)
           (forall? (λ (n) (or (= 2 n) (= 3 n) (= 5 n)))
                    (factors-rec num)
                    ))
       ))

;; Flesh this out for Problem 1b
(define (least-divisor-find num)
  (find (λ (candidate) (divisible-by? num candidate))
        num
        (let {[limit (ceiling (sqrt num))]}
        (append (list 2) (range 3 limit 2) (list limit)))
        ))

;; Flesh this out for Problem 1c
(define (factors-genlist num)
  (map second
       (genlist-apply (λ (num factor)
                        (let {[new-num (quotient num factor)]}
                        (list new-num (least-divisor-rec new-num)))) ; next
                      (λ (num factor) (= num factor)) ; done?
                      #t ; keepDoneValue?
                      (list num 
                            (least-divisor-rec num) ; seed
                             ))))

;; Flesh this out for Problem 1d
(define (factors-iterate-apply num)
  (iterate-apply (λ (n reversed-entries)
                   (let {[factor (least-divisor-rec n)]}
                   (list (quotient n factor) (cons factor reversed-entries)))); next  
                 (λ (n reversed-entries)
                   (= n (least-divisor-rec n))); done?
                 (λ (n reversed-entries)
                   (reverse (cons (least-divisor-rec n) reversed-entries))); finalize 
                 (list num null)))


;;;----------------------------------------------------------------------
;;; Solo Problem 2: Partial Reverses

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; You are given these two functions
(define (partial-reverses xs)
  (partial-reverses-tail xs '() '()))

(define (partial-reverses-tail ys rev list-rev)
  (if (null? ys)
      (cons rev list-rev)
      (partial-reverses-tail (rest ys)
                             (cons (first ys) rev)
                             (cons rev list-rev)
                             )))
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Flesh this out for Problem 2a
(define (partial-reverses-iterate xs)
  (iterate-apply (λ (lst parRevsSoFar) (list (rest lst)
                                             (cons (cons
                                                    (first lst)
                                                    (first parRevsSoFar))
                                                   parRevsSoFar))); next 
                 (λ (lst parRevsSoFar) (null? lst)); done? 
                 (λ (lst parRevsSoFar) parRevsSoFar); finalize
                 (list xs '(())); state
                 ))

;; Flesh this out for Problem 2b
(define (partial-reverses-foldl xs)
  (foldl (λ (num res) (cons (cons num (first res)) res))
         '(())
         xs))

;;;----------------------------------------------------------------------
;;; Helper Functions from PS4, PS5, and lectures

(define (genlist-apply next done? keepDoneValue? seed)
  (if (apply done? seed)
      (if keepDoneValue? (list seed) null)
      (cons seed
            (genlist-apply next done? keepDoneValue? (apply next seed)))))

(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

(define (forall? pred xs)
  (or (null? xs)
      (and (pred (first xs))
           (forall? pred (rest xs)))))

(define (exists? pred xs)
  (and (not (null? xs))
       (or (pred (first xs))
           (exists? pred (rest xs)))))

(define (find pred not-found xs)
  (if (null? xs)
      not-found
      (if (pred (first xs))
          (first xs)
          (find pred not-found (rest xs)))))
