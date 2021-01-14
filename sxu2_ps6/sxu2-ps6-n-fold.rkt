#lang racket
(provide (all-defined-out))

;; composition function 
(define (o f g) 
  (λ (x) (f (g x))))

;; identity function 
(define (id x) x)

(define (inc y) (+ y 1))
(define (dbl z) (* z 2))

(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

;; n-fold composition
;; takes a nonnegative integer n and unary function f
;; returns the n-fold composition of f
(define (n-fold n f)
  (iterate-apply (λ (num compSoFar) (list (- num 1) (o f compSoFar))) ; next
                  (λ (num compSoFar) (= num 0)) ; done?
                  (λ (num compSoFar) compSoFar) ; finalize
                  (list n id); initial state
                  ))
                  
                  
  