#lang racket

;;; PostFix interpreter that uses iterate-apply to iterate over a configuration
;;; consisting of (1) list of commands and (2) list of stack values

;;; This "fancy" version includes the following features:
;;; 1. appropriate handling of all error cases, 
;;; 2. the ability to display step-by-step execution,
;;; 3. a general, extensible way to handle arithops and relops

;; Set this to #t to turn on printing of intermediate stacks; #f to turn it off
(define display-steps? #t)

;; Run the given PostFix program on argument values, which form the initial stack
(define (postfix-run pgm args)
  (cond ((not (postfix-program? pgm))
         (error "Invalid PostFix program" pgm))
        ((not (postfix-arguments? args))
         (error "Invalid PostFix arguments" args))
        ((not (= (postfix-numargs pgm) (length args)))
         (error "Expected number of arguments does not
                 match actual number of arguments"
                (list (postfix-numargs pgm) (length args))))
        (else (postfix-exec-config-iterate (postfix-commands pgm) args))))

; CS251 helper function 
(define (iterate-apply next done? finalize state)
  (if (apply done? state)
      (apply finalize state)
      (iterate-apply next done? finalize (apply next state))))

;; Use iterate-apply to loop over a configuration state consiting of
;; (1) list of commands and (2) list of stack values
(define (postfix-exec-config-iterate cmds stk)
  (iterate-apply postfix-exec-config-one-step
                 (λ (cmds stk) (null? cmds))
                 (λ (cmds stk)
                   (cond ((null? stk)
                          (error "Stack empty at end of program"))
                         ((not (integer? (first stk)))
                          (error "Top of final stack is not an integer"))
                         (else (first stk))))
                 (list cmds stk)))

;; Perform one step of the PostFix execution on configurations.
;; Given a configuration consisting of (1) commands and (2) stack,
;; return the next configuration.
(define (postfix-exec-config-one-step cmds stk)
  (if (eq? (first cmds) 'exec)
      ; Create next configuration pair
      (list (append (first stk) (rest cmds)) (rest stk))
      (list (rest cmds) (postfix-exec-command (first cmds) stk))))       

;; Execute a non-exec command on a stack to yield a new stack.
;; So each command can be viewed as a "stack transformer"
(define (postfix-exec-command cmd stk)
  (cond ((integer? cmd) (cons cmd stk))
        ((eq? cmd 'pop)
         (if (< (length stk) 1)
             (error "pop requires nonempty stack"
                    (list cmd stk))
             (rest stk)))
        ((eq? cmd 'swap)
         (if (< (length stk) 2)
             (error "swap requires stack with at least two values" (list cmd stk))
             (cons (second stk) (cons (first stk) (rest (rest stk))))))
        ((postfix-arithop? cmd)
         (cond ((< (length stk) 2)
                (error "arithop requires two arguments" (list cmd stk)))
               ((or (not (integer? (first stk)))
                    (not (integer? (second stk))))
                (error "arithop requires two integers" (list cmd stk)))
               (else (cons ((postfix-arithop->racket-binop cmd) (second stk) (first stk))
                           (rest (rest stk))))))
        ((postfix-relop? cmd)
         (cond ((< (length stk) 2)
                (error "relop requires two arguments" (list cmd stk)))
               ((or (not (integer? (first stk)))
                    (not (integer? (second stk))))
                (error "relop requires two integers" (list cmd stk)))
               (else (cons ((postfix-relop->racket-binop cmd) (second stk) (first stk))
                           (rest (rest stk))))))
        ((eq? cmd 'nget)
         (if (< (length stk) 1)
             (error "nget requires one argument" stk)
             (let {[index (first stk)]}
               (cond ((not (integer? index))
                      (error "nget requires integer index" (list cmd stk)))
                     ((or (<= index 0)
                          (> index (length (rest stk))))
                      (error "nget index out of range" (list index (rest stk))))
                     (else 
                      (let {[valAtIndex (list-ref stk index)]}
                           ;; list-ref uses 0-based index, but haven't removed index from
                           ;; top of stack, so this works.
                        (if (not (integer? valAtIndex))
                            (error "nget can't return noninteger" valAtIndex)
                            (cons valAtIndex (rest stk)))))))))
        ((eq? cmd 'sel)
         (cond ((< (length stk) 3)
                (error "sel requires three arguments" (list cmd stk)))
               ((not (integer? (third stk)))
                (error "sel test value must be an integer" (list cmd stk)))
               (else (cons (if (= (third stk) 0) (first stk) (second stk))
                           (rest (rest (rest stk)))))))
        ((postfix-command-sequence? cmd) (cons cmd stk))
        ;; Can add clauses for new commands here
        (else (error "unrecognized command" cmd))
        ))

;;----------------------------------------------------------------------
;; General handling of arithops and relops

(define postfix-arithops
  (list (list 'add +)
        (list 'mul *)
        (list 'sub -)
        (list 'div (λ (a b)
                     (if (= b 0)
                         (error "division by 0 on" a)
                         (quotient a b))))
        (list 'rem (λ (a b)
                     (if (= b 0)
                         (error "remainder by 0 on" a)
                         (remainder a b))))
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
Commands: (2 nget 0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (3 5)
Commands: (nget 0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (2 3 5)
Commands: (0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (5 3 5)
Commands: (gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (0 5 3 5)
Commands: ((sub) (swap 1 nget mul add) sel exec)
   Stack: (1 3 5)
Commands: ((swap 1 nget mul add) sel exec)
   Stack: ((sub) 1 3 5)
Commands: (sel exec)
   Stack: ((swap 1 nget mul add) (sub) 1 3 5)
Commands: (exec)
   Stack: ((sub) 3 5)
Commands: (sub)
   Stack: (3 5)
Commands: ()
   Stack: (2)
2
|#

#|
> (postfix-run pf1 '(3 -5))
Commands: (2 nget 0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (3 -5)
Commands: (nget 0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (2 3 -5)
Commands: (0 gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (-5 3 -5)
Commands: (gt (sub) (swap 1 nget mul add) sel exec)
   Stack: (0 -5 3 -5)
Commands: ((sub) (swap 1 nget mul add) sel exec)
   Stack: (0 3 -5)
Commands: ((swap 1 nget mul add) sel exec)
   Stack: ((sub) 0 3 -5)
Commands: (sel exec)
   Stack: ((swap 1 nget mul add) (sub) 0 3 -5)
Commands: (exec)
   Stack: ((swap 1 nget mul add) 3 -5)
Commands: (swap 1 nget mul add)
   Stack: (3 -5)
Commands: (1 nget mul add)
   Stack: (-5 3)
Commands: (nget mul add)
   Stack: (1 -5 3)
Commands: (mul add)
   Stack: (-5 -5 3)
Commands: (add)
   Stack: (25 3)
Commands: ()
   Stack: (28)
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
Commands: (1 nget mul swap 1 nget mul add)
   Stack: (3 4)
Commands: (nget mul swap 1 nget mul add)
   Stack: (1 3 4)
Commands: (mul swap 1 nget mul add)
   Stack: (3 3 4)
Commands: (swap 1 nget mul add)
   Stack: (9 4)
Commands: (1 nget mul add)
   Stack: (4 9)
Commands: (nget mul add)
   Stack: (1 4 9)
Commands: (mul add)
   Stack: (4 4 9)
Commands: (add)
   Stack: (16 9)
Commands: ()
   Stack: (25)
25
|#

