#lang racket

;;; PostFix interpreter that views all commands (including exec)
;;; as stack transformers

;;; This "simple" version omits features that are included in the fancy version:
;;; 1. appropriate handling of all error cases, 
;;; 2. the ability to display step-by-step execution,
;;; 3. a general, extensible way to handle arithops and relops

(define (postfix-run pgm args)
  (let {[final-stk (postfix-exec-commands (postfix-commands pgm) args)]}
    (first final-stk)))

;; Perform the stack transform of each command from left to right,
;; starting with init-stk, to yield the final stack. 
(define (postfix-exec-commands cmds init-stk)
  (foldl postfix-exec-command init-stk cmds))

;; Execute a command on a stack to yield a new stack.
;; So each command can be viewed as a "stack transformer"
(define (postfix-exec-command cmd stk)
  (cond ((integer? cmd) (cons cmd stk))
        ((eq? cmd 'pop) (rest stk))
        ((eq? cmd 'swap) (cons (second stk) (cons (first stk) (rest (rest stk)))))
        ;; arithops
        ((eq? cmd 'add) (cons (+ (second stk) (first stk)) (rest (rest stk))))
        ((eq? cmd 'sub) (cons (- (second stk) (first stk)) (rest (rest stk))))
        ((eq? cmd 'mul) (cons (* (second stk) (first stk)) (rest (rest stk))))
        ((eq? cmd 'div) (cons (quotient (second stk) (first stk)) (rest (rest stk))))
        ((eq? cmd 'rem) (cons (remainder (second stk) (first stk)) (rest (rest stk))))
        ;; relops       
        ((eq? cmd 'lt) (cons (if (< (second stk) (first stk)) 1 0)
                             (rest (rest stk))))
        ((eq? cmd 'eq) (cons (if (= (second stk) (first stk)) 1 0)
                             (rest (rest stk))))
        ((eq? cmd 'gt) (cons (if (> (second stk) (first stk)) 1 0)
                             (rest (rest stk))))
        ;; other commands   
        ((eq? cmd 'nget)
         (let {[valAtIndex (list-ref stk (first stk))]} ; list-ref uses 0-based indexing
           (if (not (integer? valAtIndex))
               (error "nget can't return noninteger" valAtIndex)
               (cons valAtIndex (rest stk)))))
        ((eq? cmd 'sel) (cons (if (= (third stk) 0) (first stk) (second stk))
                              (rest (rest (rest stk)))))
        ((postfix-command-sequence? cmd) (cons cmd stk))
        ((eq? cmd 'exec)
         ;; Treat executable sequence as a stack transform rather than
         ;; appending the commands in the sequence to commands
         ;; portion of (stack x commands) configuration.
         (postfix-exec-commands (first stk) (rest stk)))
        (else (error "unrecognized command" cmd))))

;;----------------------------------------------------------------------
;; Postfix Syntax abstractions

(define (postfix-program? sexp)
  (and (list? sexp)
       (>= (length sexp) 2)
       (eq? (first sexp) 'postfix)
       (integer? (second sexp))
       (postfix-command-sequence? (rest (rest sexp)))))

(define (postfix-command-sequence? sexp)
  (and (list? sexp)
       (forall? postfix-command? sexp)))

(define (postfix-command? sexp)
  (or (integer? sexp)
      (postfix-command-sequence? sexp)
      (member sexp  '(add mul sub div rem ; arithops
                      lt eq gt ; relops
                      pop swap nget sel exec)) ; handle all other commands here
      ))

(define (postfix-numargs pgm) (second pgm))
(define (postfix-commands pgm) (rest (rest pgm)))

(define (postfix-arguments? sexp)
  (and (list? sexp)
       (forall? integer? sexp)))

;; Higher-order helper function from PS4
(define (forall? pred xs)
  (if (null? xs)
      #t
      (and (pred (first xs))
           (forall? pred (rest xs)))))

;;----------------------------------------------------------------------
;;; Examples

;; Sample program from lecture
(define pf1 '(postfix 2 2 nget 0 gt (sub) (swap 1 nget mul add) sel exec))

#|
> (postfix-run pf1 '(3 5))
2
> (postfix-run pf1 '(3 -5))
28
|#


 

;; Sum-of-squares program 
(define sos
  '(postfix 2 ; let's call the arguments a and b, from top down
            1 nget ; duplicate a at top of stack
            mul ; square a
            swap ; stack now has b and a^2 from top down
            1 nget mul ; square b
            add ; add b^2 + a^2 and return
            ))

#|
> (postfix-run sos '(3 4))
25
|#