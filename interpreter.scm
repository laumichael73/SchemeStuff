;Michael Folz
;Mark Gross


;main function is called by (interpret "<string of file path and name>")
;the string is in double quotes


#lang racket
(require "simpleParser.scm")

;we're going with the ((var1 var2 ...) (val1 val2...)) organization since it helps later...? hopefully?
;state is a list of two lists, m_state and m_values
;cstate is the current state, and is a list of layers
;each layer has the structure
;  ((var1 var 2...) (val1 val2...))

;the state is a list of layers

(define (reassignerror) "error- reassigning")
(define (declaringerror) "error- using a variable before declaring")
(define (assigningerror) "error- using a variable before assigning")


;----------------------------------------------------------------------------
; state manipulation methods: m_state_add, m_state_remove, and m_state_lookup
;----------------------------------------------------------------------------
(define initialstate '((()())))
(define emptylayer '(()()))

;gets the variable name list from a state
(define (vars layer) (car layer))
;gets the value list from the state
(define (vals layer) (cadr layer))
;constructs a layer from a list of vars and vals
(define (buildlayer vars vals) (list vars vals))

(define (firstelement input) (car input))
(define (restof input) (cdr input))
(define (secondelement input) (cadr input))
(define (thirdelement input) (caddr input))

;effectively same as peek, returns a layer from a state
(define (getTopLayer cstate)
  (firstelement cstate))

;returns all the layers except the top one
(define (getNextLayers cstate)
  (restof cstate))



;adds an empty layer to the input state
(define (add_layer layer cstate)
    (append (list layer) (list cstate)))

;returns the layer after we add the pair (x val) to the input layer
;throws error if the value is already used or declared
(define (addto_layer x val layer)
  (cond
    ((not (null? (layer_lookup x layer))) (reassignerror))
    (else (buildlayer (cons x (vars layer)) (cons val (vals layer))))))

;removes that var from the state, and the associated values with that label
;doesn't assume that there is only one instance of the var, removes all of them
(define (removefrom_layer var layer)
  (cond
    ((null? (vars layer)) '())
    ((equal? var (firstelement (vars layer))) (removefrom_layer var (buildlayer (restof (vars layer)) (restof (vals layer)))))
    (else
     (appendto_layer (firstelement (vars layer)) (firstelement (vals layer)) (removefrom_layer var (buildlayer (restof (vars layer)) (restof (vals layer))))))))
;helper for removefrom, not to be called by anything else since it doesn't check
(define (appendto_layer x val layer)
  (cond
    ((null? layer) (buildlayer (list x) (list val)))
    (else (buildlayer (cons x (vars layer)) (cons val (vals layer))))))

;returns the vals associated with the var in the given layer of the state
;if there isn't a val, then returns the empty list
(define (layer_lookup variable layer)
  (cond
    ((null? (vars layer)) '())
    ((equal? variable (firstelement (vars layer))) (firstelement (vals layer)))
    (else (layer_lookup variable (buildlayer (restof (vars layer)) (restof (vals layer)))))))


(define (m_state_lookup var state)
    (if (null? state) '()
    (if (null? (layer_lookup var (getTopLayer state))) (layer_lookup var (getTopLayer state))
        (m_state_lookup var (getNextLayers (state))))))

(define (m_state_add var val cstate)
  (addto_layer var val (getTopLayer cstate)))

(define (m_state_remove var cstate)
  (removefrom_layer var (getTopLayer cstate)))

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
  (read (parser filepathandname) initialstate))

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



(define testlayer '((t v g y s g thsi x) (1 2 3 4 5 6 7 8)))
