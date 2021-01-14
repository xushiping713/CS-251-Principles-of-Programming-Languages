#lang racket
(provide (all-defined-out))

;; helper function
(define (prob1-map-cons x yss)
  (if (null? yss)
      null
      (cons (cons x (first yss)) (prob1-map-cons x (rest yss)))))


;; takes a list as its single argument
;; returns a list of all its prefixes ordered from shortest to longest
(define (prefixes xs)
  (if (null? xs)
      '(())
      (cons '() (prob1-map-cons (first xs) (prefixes (rest xs))))))

;; takes a list of integers as its single argument
;; returns a triple whose three elements are (1) sum of the numbers in the list;
;; (2) maximum of the numbers in the list; (3) a list of squares of all the even numbers in the list
(define (sum-max-squaresEvens xs)
  (if (null? xs)
      (list 0 -inf.0 '())
      (let {[subresults (sum-max-squaresEvens (rest xs))]}
      (list
       (+ (first xs) (first subresults))
       (max (first xs) (second subresults))
       (if (even? (first xs))
           (cons (* (first xs) (first xs)) (third subresults))
           (third subresults))))))

;; takes as its single argument a set
;; returns a list of all subsets of a given set
(define (subsets xs)
  (if (null? xs)
      '(())
      (let {[subsets-rest (subsets (rest xs))]}
      (append subsets-rest (prob1-map-cons (first xs) subsets-rest)))))

;; takes a list of numbers
;; returns a list in which each nonempty sublist is the result of scaling all numbers in
;; the corresponding nonempty sublist in the result of suffixes by its first element
(define (weighted-suffixes xs)
  (if (null? xs)
      '(())
      (cons (map-scale (first xs) xs) (weighted-suffixes (rest xs)))))

;; helper function - scales every element in ns by factor
(define (map-scale factor ns)
  (if (null? ns)
      null
      (cons (* factor (first ns))
            (map-scale factor (rest ns)))))


  