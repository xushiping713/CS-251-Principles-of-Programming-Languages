#lang racket
(provide (all-defined-out))

(define (map-cons x ys)
  (map (curry cons x) ys))

(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      null
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;; takes a list of pairs
;; returns a list of two lists that if zipped together with zip, would yield the original pairs
(define (unzip pairs)
  (foldr (λ (p l) (list
                   (cons (car p) (first l))
                   (cons (cdr p) (second l))
                   ))
         '(() ())
         pairs))

;; takes a list as its single argument
;; returns a list of all its prefixes order from shortest to longest
(define (prefixes xs)
  (foldr (λ (x l) (cons '() (map-cons x l)))
         '(())
         xs))

;; takes a list of integers
;; returns a triple whose three elements are (1) sum of the numbers in the list;
;; (2) maximum of the numbers in the list; (3) a list of squares of all the even numbers in the list
(define (sum-max-squaresEvens ints)
  (foldr (λ (int l) (list
                     (+ int (first l))
                     (max int (second l))
                     (if (even? int) (cons (* int int) (third l)) (third l))
                     ))
         '(0 -inf.0 ())
         ints))

;; takes a set if integers
;; returns a list of all subsets of a given set
(define (subsets set)
  (foldr (λ (int l) (append l (map-cons int l)))
         '(())
         set))

;; takes a list as its single argument
;; returns a list of all of its suffixes ordered from longest to shortest
(define (suffixes xs)
  (foldr (λ (x l) (cons (cons x (first l)) l))
         '(())
         xs))

;; takes a list of numbers
;; returns a list in which each nonempty sublist is the result of scaling all numbers in
;; the corresponding nonempty sublist in the result of suffixes by its first element
(define (weighted-suffixes nums)
  (foldr-ternop (λ (f rs l) (cons (map (λ (r) (* f r)) (cons f rs)) l))
                '(())
                nums))
                                  
(define (foldr-ternop ternop null-value xs)
  (if (null? xs)
      null-value
      (ternop (first xs)
              (rest xs)
              (foldr-ternop ternop null-value (rest xs)))))
         

             

          


