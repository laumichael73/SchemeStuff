;Michael Folz
;Mark Gross

#lang racket
(require "simpleParser.scm")

;we're going with the ((var1 var2 ...) (val1 val2...)) organization since it helps later...? hopefully?
;state is a list of two lists, m_state and m_values
;cstate- "current state" has structure (m_state m_values)
;m_state has structure (var1... varn)
;m_values has structure (val1... valn)

(define error1 "error- reassigning")
(define error2 "error- using a variable before declaring")
(define error3 "error- using a variable before assigning")


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
    ((eq? (m_state_lookup var cstate) '()) (state_append_tofront var val cstate))
    (else error1)))

;removes that var from the state, and the associated values with that label
;doesn't assume that the value is used once, will remove all instances of that variable
(define (m_state_remove var cstate)
	(cond
          ((null? (m_state cstate)) '(()()) )
          ;if it's eq then recurse on (buildstate the next var, and the next val)
          ((eq? var (car (m_state cstate))) (m_state_remove var (recursestate cstate)))
	(else (state_append_tofront (car (m_state cstate)) (car (m_values cstate)) (m_state_remove var (recursestate cstate))))))

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


(define (recursestate cstate) (buildstate (cdr (m_state cstate)) (cdr (m_values cstate))))
;------------------------------------------------------------------------------------------
;interpreter methods
;--------------------------------------------------------------------------------------------
;needs to check for:

;Test 12: This code should give an error (using before declaring).
;Test 13: This code should give an error (using before assigning).
;Test 14: This code should give an error (redefining).  This is not a required error, but it would be nice if you
;could catch these.

;return true and false rather than #t and #f


;return
;(define (return var cstate))

;when a function is declared, before it is used, it is associated with the value 'declared rather than a number
;m_state_add already checks that the variable isn't already declared
;var
(define (declare var cstate) (m_state_add var 'declared cstate))
    
;&&

;||


;%
;! (something)

;=
(define (equals var val cstate)
  (cond
    ((equal? (m_state_lookup var cstate) '(declared)) (m_state_add var val (m_state_remove var cstate)))
    (else error2)))

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

(define (interpret lis cstate)
  (cond
    ((null? lis) lis)
    ((null? (car lis)) lis)
    ;here the car of lis isn't a number, so check what it is
    ((number? (car lis)) (car lis))
    ((list? (car lis)) (interpret (car lis) cstate))
    ((equal? '(return) (car lis)) (interpret (cdr lis) cstate))
    ((equal? '(-) (car lis)) (- (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))
    ((equal? '(/) (car lis)) (/ (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))
    ((equal? '(*) (car lis)) (* (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))
    ((equal? '(+) (car lis)) (+ (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))
    ((equal? '(<) (car lis)) (< (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))
    ((equal? '(>) (car lis)) (> (interpret (cadr lis) cstate) (interpret (caddr lis) cstate)))

    (else (interpret (cdr lis) cstate))))
    







;---------------------------------------------------------------------------------
;ignore this stuff, it's only for testing to make sure stuff works
;-----------------------------------------------------------------------------------
(define teststate  '((x y z) (1 2 3)))
(define nullstate '(()()))



(define (test1) (appendit (parser "testfiles/test1") (interpret (parser "testfiles/test1") nullstate)))
(define (test2) (appendit (parser "testfiles/test2") (interpret (parser "testfiles/test2") nullstate)))
(define (test3) (appendit (parser "testfiles/test3") (interpret (parser "testfiles/test3") nullstate)))
(define (test4) (appendit (parser "testfiles/test4") (interpret (parser "testfiles/test4") nullstate)))
(define (test5) (appendit (parser "testfiles/test5") (interpret (parser "testfiles/test5") nullstate)))
(define (test6) (appendit (parser "testfiles/test6") (interpret (parser "testfiles/test6") nullstate)))
(define (test7) (appendit (parser "testfiles/test7") (interpret (parser "testfiles/test7") nullstate)))
(define (test8) (appendit (parser "testfiles/test8") (interpret (parser "testfiles/test8") nullstate)))
(define (test9) (appendit (parser "testfiles/test9") (interpret (parser "testfiles/test9") nullstate)))
(define (test10) (appendit (parser "testfiles/test10") (interpret (parser "testfiles/test10") nullstate) ))
(define (test11) (appendit (parser "testfiles/test11") (interpret (parser "testfiles/test11") nullstate) ))
(define (test12) (appendit (parser "testfiles/test12") (interpret (parser "testfiles/test12") nullstate) ))
(define (test13) (appendit (parser "testfiles/test13") (interpret (parser "testfiles/test13") nullstate) ))
(define (test14) (appendit (parser "testfiles/test14") (interpret (parser "testfiles/test14") nullstate) ))
(define (test15) (appendit (parser "testfiles/test15") (interpret (parser "testfiles/test15") nullstate) ))
(define (test16) (appendit (parser "testfiles/test16") (interpret (parser "testfiles/test16") nullstate) ))
(define (test17) (appendit (parser "testfiles/test17") (interpret (parser "testfiles/test17") nullstate) ))
(define (test18) (appendit (parser "testfiles/test18") (interpret (parser "testfiles/test18")nullstate) ))
(define (test19) (appendit (parser "testfiles/test19") (interpret (parser "testfiles/test19")nullstate) ))
(define (test20) (appendit (parser "testfiles/test20") (interpret (parser "testfiles/test20")nullstate) ))
(define (test21) (appendit (parser "testfiles/test21") (interpret (parser "testfiles/test21")nullstate) ))
(define (test22) (appendit (parser "testfiles/test22") (interpret (parser "testfiles/test22")nullstate) ))
(define (test23) (appendit (parser "testfiles/test23") (interpret (parser "testfiles/test23")nullstate) ))
(define (test24) (appendit (parser "testfiles/test24") (interpret (parser "testfiles/test24")nullstate) ))
(define (test25) (appendit (parser "testfiles/test25") (interpret (parser "testfiles/test25")nullstate) ))
(define (test26) (appendit (parser "testfiles/test26") (interpret (parser "testfiles/test26")nullstate) ))
(define (test27) (appendit (parser "testfiles/test27") (interpret (parser "testfiles/test27")nullstate) ))
(define (test28) (appendit (parser "testfiles/test28") (interpret (parser "testfiles/test28")nullstate) ))

(define (interpret1) (interpret (parser "testfiles/test1") nullstate))
(define (interpret2) (interpret (parser "testfiles/test2") nullstate))
(define (interpret3) (interpret (parser "testfiles/test3") nullstate))
(define (interpret4) (interpret (parser "testfiles/test4") nullstate))
(define (interpret5) (interpret (parser "testfiles/test5") nullstate))
(define (interpret6) (interpret (parser "testfiles/test6") nullstate))
(define (interpret7) (interpret (parser "testfiles/test7") nullstate))
(define (interpret8) (interpret (parser "testfiles/test8") nullstate))
(define (interpret9) (interpret (parser "testfiles/test9") nullstate))
(define (interpret10) (interpret (parser "testfiles/test10") nullstate))
(define (interpret11) (interpret (parser "testfiles/test11") nullstate))
(define (interpret12) (interpret (parser "testfiles/test12") nullstate))
(define (interpret13) (interpret (parser "testfiles/test13") nullstate))
(define (interpret14) (interpret (parser "testfiles/test14") nullstate))
(define (interpret15) (interpret (parser "testfiles/test15") nullstate))
(define (interpret16) (interpret (parser "testfiles/test16") nullstate))
(define (interpret17) (interpret (parser "testfiles/test17") nullstate))
(define (interpret18) (interpret (parser "testfiles/test18") nullstate))
(define (interpret19) (interpret (parser "testfiles/test19") nullstate))
(define (interpret20) (interpret (parser "testfiles/test20") nullstate))
(define (interpret21) (interpret (parser "testfiles/test21") nullstate))
(define (interpret22) (interpret (parser "testfiles/test22") nullstate))
(define (interpret23) (interpret (parser "testfiles/test23") nullstate))
(define (interpret24) (interpret (parser "testfiles/test24") nullstate))
(define (interpret25) (interpret (parser "testfiles/test25") nullstate))
(define (interpret26) (interpret (parser "testfiles/test26") nullstate))
(define (interpret27) (interpret (parser "testfiles/test27") nullstate))
(define (interpret28) (interpret (parser "testfiles/test28") nullstate))
