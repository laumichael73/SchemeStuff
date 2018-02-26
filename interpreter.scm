;Michael Folz
;Mark Gross


;main function is called by (interpret "<string of file path and name>")
;the string is in double quotes

;it works for test cases 1 and 2, but fails for all the rest

#lang racket
(require "simpleParser.scm")

;we're going with the ((var1 var2 ...) (val1 val2...)) organization since it helps later...? hopefully?
;state is a list of two lists, m_state and m_values
;cstate- "current state" has structure (m_state m_values)
;m_state has structure (var1... varn)
;m_values has structure (val1... valn)

(define (reassignerror) "error- reassigning")
(define (declaringerror) "error- using a variable before declaring")
(define (assigningerror) "error- using a variable before assigning")


;----------------------------------------------------------------------------
; state manipulation methods: m_state_add, m_state_remove, and m_state_lookup
;----------------------------------------------------------------------------
(define (initialstate) '(()()))

;gets the variable name list from a state
(define (m_state cstate) (car cstate))
;gets the value list from the state
(define (m_values cstate) (cadr cstate))
;constructs a state from a list of vars and vals
(define (buildstate vars vals) (list vars vals))

(define (firstelement input) (car input))
(define (secondelement input) (cadr input))
(define (thirdelement input) (caddr input))

;adds the pair (var, val) to the state
(define (state_append_tofront var val cstate)
	(buildstate (cons var (m_state cstate))(cons val (m_values cstate))))

;iterates through the next variable name and its value
(define (recursestate cstate) (buildstate (cdr (m_state cstate)) (cdr (m_values cstate))))

;add a value to the state
(define (m_state_add var val cstate)
  (cond
    ;check if it's already defined, if not we're good so add it
    ((eq? (m_state_lookup var cstate) '()) (state_append_tofront var val cstate))
    (else reassignerror)))

;removes that var from the state, and the associated values with that label
;doesn't assume that the value is used once, will remove all instances of that variable - not sure why we need this functionality but still
(define (m_state_remove var cstate)
	(cond
          ((null? (m_state cstate)) '(()()) )
          ;if it's eq then recurse and ignore the (var, val) pair
          ((eq? var (car (m_state cstate))) (m_state_remove var (recursestate cstate)))
	(else (state_append_tofront (car (m_state cstate)) (car (m_values cstate)) (m_state_remove var (recursestate cstate))))))

;returns a list of vals associated with the var
;if there isn't a val, then returns the empty list
(define (m_state_lookup var cstate)
	(cond
	;if it's null then we got through the whole thing without finding the var, so it's not there
          ((null? (m_state cstate)) '()) ;didn't find that var in the state
          ((eq? var (car (m_state cstate))) (cons (car(m_values cstate))  (m_state_lookup var (recursestate cstate))))
          (else (m_state_lookup var (recursestate cstate)))))


;------------------------------------------------------------------------------------------
;interpreter methods
;--------------------------------------------------------------------------------------------

;return true and false rather than #t and #f
;(define (return var cstate))


;when a variable is declared, before it is used, it is associated with the value 'declared rather than a number
;m_state_add already checks that the variable isn't already declared, throws error
(define (declare var cstate) (m_state_add var 'declared cstate))


;checks that the value is already declared to something
(define (equals var val cstate)
  (cond
    ;check that the value for the variable is 'declared or some number, ie it's already declared
    ((not (null? (m_state_lookup var cstate))) (m_state_add var val (m_state_remove var cstate)))
    ;if its not declared then need to declare before using
    (else (declaringerror))))


;doesn't handle the following things:
;&&
;||
;! (something)
;if
;else
;while (cond) do ()

(define (interpret filepathandname)
  (read (parser filepathandname) (initialstate)))

(define (read  input cstate)
  (cond
    ((null?  input)  input)
    ((number?  input)  input)
    ((null? (firstelement  input)) (firstelement  input))
    ((number? (firstelement  input)) (firstelement  input))
    ((and (list? (firstelement input)) (not (null?(cdr input)))) (read (firstelement input) cstate) (read (cdr input) cstate))
    ((list? (firstelement input)) (read (firstelement input) cstate))

    ;test for unary operators
    
    ;(operator <input1> <input2>)
    ((equal? '- (firstelement input)) (- (read (secondelement input) cstate) (read (thirdelement  input) cstate)))
    ((equal? '/ (firstelement  input)) (floor (/ (read (secondelement input) cstate) (read (thirdelement  input) cstate))))
    ((equal? '* (firstelement  input)) (* (read (secondelement input) cstate) (read (thirdelement input) cstate)))
    ((equal? '+ (firstelement  input)) (+ (read (secondelement input) cstate) (read (thirdelement input) cstate)))
    ((equal? '< (firstelement  input)) (< (read (secondelement input) cstate) (read (thirdelement input) cstate)))
    ((equal? '> (firstelement  input)) (> (read (secondelement input) cstate) (read (thirdelement input) cstate)))
    ((equal? '% (firstelement  input)) (modulo (read (secondelement input) cstate) (read (thirdelement  input) cstate)))
    ;if the first statement is (var ....)
    ((equal? 'var (firstelement input)) (read (cdr input) (declarevariable input cstate)))
    ;if the first statement is (return ...)
    ((equal? 'return (firstelement input)) (read (cdr input) cstate))
    (else (read (cdr input) cstate))))
   


;returns the updated state after declaring variable, called by read
(define (declarevariable input cstate)
  (cond
     ;if it's a unary operator (var x)
     ((null? (cddr input)) (declare (secondelement input) cstate))
     ;else it's a binary (var x 10)
     (else (equals (secondelement input) (thirdelement input) (declare (secondelement input) cstate)))))

;(return <expression>)
(define (returnvalue input cstate)
  (cond
    ((number? input) input)
    ((isVariable input cstate) (m_state_lookup input cstate ))
    (read input cstate)))

(define (isVariable var cstate)
  (if (not (null? (m_state_lookup var cstate))) #t
      #f))





;if it's none of those characters, throw error

(define m.value.int
  (lambda (in)
    (cond
      ((number? in) in)
      ((eq? (operator in) '+) (+ (m.value.int (operand1 in)) (operand2 in )))
      ((eq? (operator in) '-) (- (m.value.int (operand1 in)) (operand2 in )))
      ((eq? (operator in) '*) (* (m.value.int (operand1 in) (operand2 in ))))
      ((eq? '/ (operator in)) (quotient (m.value.int (operand1 in) (m.value.int(operand2 in )))))
      ((eq? '% (operator in)) (remainder (m.value.int (operand1 in) (m.value.int (operand2 in )))))
      (else (error "Undefined Operator")))))

(define operator
  (lambda (e)
    (car e)))
(define operand1 cadr)
(define operand2 caddr)





(define (unaryoperators  input cstate)
  (cond
    ((equal? 'return (firstelement  input)) (read (cdr input) cstate))
    ((equal? '- (firstelement  input)) (read (cdr  input) cstate))
    ((equal? '! (firstelement  input)) (read (cdr  input) cstate))
    ((equal? 'var (firstelement  input) (read (cdr  input) (declare (secondelement  input) cstate))))
    (else (print "here"))))


;IT
