#lang racket

;;; PostFix interpreter that uses tail recursion to iterate over a configuration
;;; consisting of (1) list of commands and (2) list of stack values

;;; This "simple" version omits features that are included in the fancy version:
;;; 1. appropriate handling of all error cases, 
;;; 2. the ability to display step-by-step execution,
;;; 3. a general, extensible way to handle arithops and relops

;; Run the given PostFix program on argument values, which form the initial stack
(define (postfix-run pgm args)
  (postfix-exec-config-iterate (postfix-commands pgm) args))

(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

;; Use iterate-apply to loop over a configuration state consiting of
;; (1) list of commands and (2) list of stack values
(define (postfix-exec-config-iterate cmds stk)
  (iterate-apply postfix-exec-config-one-step
                 (λ (cmds stk) (null? cmds))
                 (λ (cmds stk) (first stk))
                 (list cmds stk)))

;; Perform one step of the PostFix execution on configurations.
;; Given a configuration consisting of (1) commands and (2) stack,
;; return the next configuration.
(define (postfix-exec-config-one-step cmds stk)
  (begin (if display-steps? ; Only print intermediate stack if display-steps? is #t
             (printf "Commands: ~a\n   Stack: ~a\n" cmds stk)
             'do-nothing)
         (if (eq? (first cmds) 'exec)
             (cond ((< (length stk) 1)
                    (error "exec requires nonempty stack" (list cmds stk)))
                   ((not (postfix-command-sequence? (first stk)))
                       (error "exec requires executable sequence on top of stack" (list cmds stk)))
                   (else
                    ; Create next configuration pair
                    (list (append (first stk) (rest cmds)) (rest stk))))
             ; Create next configuration pair
             (list (rest cmds) (postfix-exec-command (first cmds) stk)))))

;; Use tail recursion to loop over a configuration state consiting of
;; (1) list of commands and (2) list of stack values
(define (postfix-exec-commands-tail cmds stk)
  (cond ((null? cmds) (first stk)) ; Return top of stack at end of program
        ((eq? (first cmds) 'exec)
         (postfix-exec-commands-tail (append (first stk) (rest cmds))
                                     (rest stk)))
        (else (postfix-exec-commands-tail (rest cmds)
                                          (postfix-exec-command (first cmds) stk)))))

;; Execute a non-exec command on a stack to yield a new stack.
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
        (else (error "unrecognized command" cmd))))

;;----------------------------------------------------------------------
;; General handling of arithops and relops

(define postfix-arithops
  (list (list 'add +)
        (list 'mul *)
        (list 'sub -)
        (list 'div quotient)
        (list 'rem remainder)
        ;; Could add more arithops here
        ))

(define (postfix-arithop? cmd)
  (assoc cmd postfix-arithops))

(define (postfix-arithop->racket-binop arithop)
  (second (assoc arithop postfix-arithops)))

(define postfix-relops
  (list (list 'lt <)
        (list 'eq =)
        (list 'gt >)
        ;; Could add more relops here
        ))
        
(define (postfix-relop? cmd)
  (assoc cmd postfix-relops))

(define (postfix-relop->racket-binop relop)
  (let {[boolop (second (assoc relop postfix-relops))]}
    (lambda (x y) (if (boolop x y) 1 0))))

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
      (postfix-arithop? sexp) ; defaults are: add mul sub div rem (but could add more)
      (postfix-relop? sexp) ; defaults are: lt eq gt (but could add more)
      (member sexp  '(pop swap nget sel exec)) ; handle all other commands here
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

