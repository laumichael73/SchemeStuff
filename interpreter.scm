;Michael Folz
;Mark Gross

#lang racket
(require "simpleParser.scm")

;we're going with the ((var1 var2 ...) (val1 val2...)) organization since it helps later...? hopefully?
;m_state (var1... varn)
;m_values (val1... valn)
;cstate (m_state m_values)

;(define (m_state) (car cstate))
;(define (m_values) (cadr cstate))

;add a value to the state
(define (m_state_add var val state values)
	(cond
		((null? state) (cons state var) (cons values val))
		(else (m_state_add var val (cdr state) (cdr values)))))

;removes that var and that val from state and values respectively
(define (m_state_remove var state values)
	(cond
		((null? state) '() '())
		((eq? var (car state)) (cdr state) (cdr values))
		(else (m_state_remove var (cdr state) (cdr values)))))

;returns the val associated with the var
(define (m_state_lookup var state values)
	(cond
		((null? state) '(() ()) )
		((eq? (car state)) (car values))
		(else (m_state_lookup var (cdr state) (cdr values)))))
 

;needs to check for:

;Test 12: This code should give an error (using before declaring).

;Test 13: This code should give an error (using before assigning).

;Test 14: This code should give an error (redefining).  This is not a required error, but it would be nice if you
;could catch these.

;return true and false rather than #t and #f



;need to write:

;return
;var

;&&
;||


;%
;! (something)
;=
;- (both subtraction and unary)
;/
;*
;+
;>
;<

;() units



;if
;else

;while (cond 



(define (testeverything)
(list 
(cons (parser "testfiles/test1") (interpret (parser "testfiles/test1")))
(cons (parser "testfiles/test2") (interpret (parser "testfiles/test2")))
(cons (parser "testfiles/test3") (interpret (parser "testfiles/test3")))
(cons (parser "testfiles/test4") (interpret (parser "testfiles/test4")))
(cons (parser "testfiles/test5") (interpret (parser "testfiles/test5")))
(cons (parser "testfiles/test6") (interpret (parser "testfiles/test6")))
(cons (parser "testfiles/test7") (interpret (parser "testfiles/test7")))
(cons (parser "testfiles/test8") (interpret (parser "testfiles/test8")))
(cons (parser "testfiles/test9") (interpret (parser "testfiles/test9")))
(cons (parser "testfiles/test10") (interpret (parser "testfiles/test10")))
(cons (parser "testfiles/test11") (interpret (parser "testfiles/test11")))
(cons (parser "testfiles/test12") (interpret (parser "testfiles/test12")))
(cons (parser "testfiles/test13") (interpret (parser "testfiles/test13")))
(cons (parser "testfiles/test14") (interpret (parser "testfiles/test14")))
(cons (parser "testfiles/test15") (interpret (parser "testfiles/test15")))
(cons (parser "testfiles/test16") (interpret (parser "testfiles/test16")))
(cons (parser "testfiles/test17") (interpret (parser "testfiles/test17")))
(cons (parser "testfiles/test18") (interpret (parser "testfiles/test18")))
(cons (parser "testfiles/test19") (interpret (parser "testfiles/test19")))
(cons (parser "testfiles/test20") (interpret (parser "testfiles/test20")))
(cons (parser "testfiles/test21") (interpret (parser "testfiles/test21")))
(cons (parser "testfiles/test22") (interpret (parser "testfiles/test22")))
(cons (parser "testfiles/test23") (interpret (parser "testfiles/test23")))
(cons (parser "testfiles/test24") (interpret (parser "testfiles/test24")))
(cons (parser "testfiles/test25") (interpret (parser "testfiles/test25")))
(cons (parser "testfiles/test26") (interpret (parser "testfiles/test26")))
(cons (parser "testfiles/test27") (interpret (parser "testfiles/test27")))
(cons (parser "testfiles/test28") (interpret (parser "testfiles/test28"))))

