#lang racket

(define (atom? x)
  (or (number? x) (boolean? x) (string? x) (symbol? x)))

(define (sexp-num-atoms sexp)
  (if (atom? sexp)
      1
      (foldr + 0 (map sexp-atoms sexp))))

(define (sexp-atoms sexp)
  (if (atom? sexp)
      (list sexp)
      (foldr append null (map sexp-atoms sexp))))

;;----------------------------------------------------------------------
;; Put your definition of deep-reverse here:

(define (deep-reverse sexp)
  (if (atom? sexp)
      sexp
      (reverse (map deep-reverse sexp))))
      
