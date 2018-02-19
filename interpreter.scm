;Michael Folz
;Mark Gross

#lang racket
(require "simpleParser.scm")

;we're going with the ((var1 var2 ...) (val1 val2...)) organization since it helps later...? hopefully?
;state is a list of two lists, m_state and m_values
;cstate- "current state" has structure (m_state m_values)
;m_state has structure (var1... varn)
;m_values has structure (val1... valn)

(define error1 "reassigning, what are you doing")
(define error2 "need to declare a variable before you can use it, stop writing bad code")
(define error3 "using before assigning you cottonheaded ninny muggins")


;----------------------------------------------------------------------------
; state manipulation methods: m_state_add, m_state_remove, and m_state_lookup
;----------------------------------------------------------------------------

;gets the state list from a state
(define (m_state cstate) (car cstate))
;gets the value list from the state
(define (m_values cstate) (cadr cstate))
;constructs a state from a list of vars and vals
;TODO check they're the same length? might become a problem in the future, idk
(define (buildstate vars vals) (list vars vals))

;add a value to the state
(define (m_state_add var val cstate)
  (cond
    ((eq? (m_state_lookup var cstate) '(declared)) (state_append_tofront var val cstate))
    (else error1)))

;removes that var from the state, and the associated values with that label
;doesn't assume that the value is used once, will remove all instances of that variable
(define (m_state_remove var cstate)
	(cond
          ((null? (m_state cstate)) (buildstate '() '()))
          ;if it's eq then recurse on (buildstate the next var, and the next val)
          ((eq? var (car (m_state cstate))) (m_state_remove var ((recursestate cstate))))
	(else (state_append_tofront (car (m_state cstate)) (car (m_values cstate)) (m_state_remove var ((recursestate cstate)))))))

;returns a list of vals associated with the var
(define (m_state_lookup var cstate)
	(cond
	;if it's null then we got through the whole thing without finding the var, so it's not there
          ((null? (m_state cstate)) '()) ;didn't find that var in the state
          ((eq? var (car (m_state cstate))) (cons (car(m_values cstate))  (m_state_lookup var (recursestate cstate))))
          (else (m_state_lookup var (recursestate cstate)))))

;cps append taken from class on 2/16
(define (appendit l1 l2)
  (if (null? l1) (mycont l2)
      (cons (car l1) (appendit (cdr l1) l2))))
(define mycont (lambda (v) v))

(define (state_append_tofront var val cstate)
	(buildstate (cons var (m_state cstate))(cons val (m_values cstate))))


(define (recursestate cstate) (buildstate (cdr (m_state cstate)) (cdr (m_values buildstate))))
;------------------------------------------------------------------------------------------
;interpreter methods
;--------------------------------------------------------------------------------------------
;needs to check for:

;Test 12: This code should give an error (using before declaring).
;Test 13: This code should give an error (using before assigning).
;Test 14: This code should give an error (redefining).  This is not a required error, but it would be nice if you
;could catch these.

;return true and false rather than #t and #f



;need to write:

;return
(define (return var cstate) (m_state_lookup var cstate))

;when a function is declared, before it is used, it is associated with the value 'declared rather than a number
;TODO check when we're doing operations that the value is assigned a number and not declared
;TODO check that the value is't already declared
;var
(define (declare var cstate) (m_state_add var 'declared cstate))

;&&

;||


;%
;! (something)

;=
(define (equals var val cstate) (m_state_add var val cstate))

;- (both subtraction and negative)
;/
;*
;+
;>
;<

;() units



;if
;else

;while (cond) do ()

(define (interpret lis)
   '(result) )


(define (teststate) (buildstate '(x y z) '(1 2 3)))



(define (test1) (appendit (parser "testfiles/test1") (interpret (parser "testfiles/test1")) ))
(define (test2) (appendit (parser "testfiles/test2") (interpret (parser "testfiles/test2")) ))
(define (test3) (appendit (parser "testfiles/test3") (interpret (parser "testfiles/test3")) ))
(define (test4) (appendit (parser "testfiles/test4") (interpret (parser "testfiles/test4")) ))
(define (test5) (appendit (parser "testfiles/test5") (interpret (parser "testfiles/test5")) ))
(define (test6) (appendit (parser "testfiles/test6") (interpret (parser "testfiles/test6")) ))
(define (test7) (appendit (parser "testfiles/test7") (interpret (parser "testfiles/test7")) ))
(define (test8) (appendit (parser "testfiles/test8") (interpret (parser "testfiles/test8")) ))
(define (test9) (appendit (parser "testfiles/test9") (interpret (parser "testfiles/test9")) ))
(define (test10) (appendit (parser "testfiles/test10") (interpret (parser "testfiles/test10")) ))
(define (test11) (appendit (parser "testfiles/test11") (interpret (parser "testfiles/test11")) ))
(define (test12) (appendit (parser "testfiles/test12") (interpret (parser "testfiles/test12")) ))
(define (test13) (appendit (parser "testfiles/test13") (interpret (parser "testfiles/test13")) ))
(define (test14) (appendit (parser "testfiles/test14") (interpret (parser "testfiles/test14")) ))
(define (test15) (appendit (parser "testfiles/test15") (interpret (parser "testfiles/test15")) ))
(define (test16) (appendit (parser "testfiles/test16") (interpret (parser "testfiles/test16")) ))
(define (test17) (appendit (parser "testfiles/test17") (interpret (parser "testfiles/test17")) ))
(define (test18) (appendit (parser "testfiles/test18") (interpret (parser "testfiles/test18")) ))
(define (test19) (appendit (parser "testfiles/test19") (interpret (parser "testfiles/test19")) ))
(define (test20) (appendit (parser "testfiles/test20") (interpret (parser "testfiles/test20")) ))
(define (test21) (appendit (parser "testfiles/test21") (interpret (parser "testfiles/test21")) ))
(define (test22) (appendit (parser "testfiles/test22") (interpret (parser "testfiles/test22")) ))
(define (test23) (appendit (parser "testfiles/test23") (interpret (parser "testfiles/test23")) ))
(define (test24) (appendit (parser "testfiles/test24") (interpret (parser "testfiles/test24")) ))
(define (test25) (appendit (parser "testfiles/test25") (interpret (parser "testfiles/test25")) ))
(define (test26) (appendit (parser "testfiles/test26") (interpret (parser "testfiles/test26")) ))
(define (test27) (appendit (parser "testfiles/test27") (interpret (parser "testfiles/test27")) ))
(define (test28) (appendit (parser "testfiles/test28") (interpret (parser "testfiles/test28")) ))
