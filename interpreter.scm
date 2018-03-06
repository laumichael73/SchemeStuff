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
;   (layer1 layer2 ... lastlayer)


(define (reassignerror) "error- reassigning")
(define (declaringerror) "error- using a variable before declaring")
(define (assigningerror) "error- using a variable before assigning")
(define (undefinedexpressionerror) "error- undefined expression")
(define (undefinedunaryoperator) "error- undefined unary operator")


;----------------------------------------------------------------------------
; state manipulation methods: m_state_add, m_state_remove, and m_state_lookup
;----------------------------------------------------------------------------
;lookup returns a "true" or "false" if that's what the value is defined as


;gets the variable name list from a state
(define (vars layer) (car layer))
;gets the value list from the state
(define (vals layer) (cadr layer))
;constructs a layer from a list of vars and vals
(define (buildlayer vars vals) (list vars vals))


(define initialstate '((()())))
(define emptylayer '(()()))
(define testlayer '((t v g y s g thsi x) (1 2 3 4 5 6 7 8)))
(define teststate (list testlayer)) ;TODO remove this bad boy

(define firstelement car)
(define restof cdr)
(define secondelement cadr)
(define thirdelement caddr)
(define operator car )
(define operand1 cadr)
(define operand2 caddr)


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

;for looking up a value
;TODO fix this
(define (m_state_lookup var state)
    (cond
      ((null? state) '())
      ((not (null? (layer_lookup var (getTopLayer state)))) (layer_lookup var (getTopLayer state)))
      (else (m_state_lookup var (getNextLayers state)))))

(define (m_state_add var val cstate)
  (addto_layer var val (getTopLayer cstate)))

(define (m_state_remove var cstate)
  (removefrom_layer var (getTopLayer cstate)))

;not sure how to do this one TODO remove this?
  (define (m_boolean expression cstate)
    (cond
      ((null? (getTopLayer cstate)))))


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
    (else (read input cstate))))

;check if something is a variable or not
;need this since we can declare variables that aren't just letters
(define (isVariable var cstate)
  (if (not (null? (m_state_lookup var cstate))) #t
    #f))





;need to add:
;everything from 2
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

    ;if the expression needs to be evaluated to int
      ;call intevaluate

    ;if the expression needs to be evaluated to boolean
      ;call booleanevaluate

    ;if the first statement is (var ....)
    ((equal? 'var (firstelement input)) (read (cdr input) (declarevariable input cstate)))
    ;if the first statement is (return ...)
    ((equal? 'return (firstelement input)) (read (cdr input) cstate))
    (else (read (cdr input) cstate))))




;TODO return 'true' or 'false' rather than #t or #f
(define (booleanevaluate expression cstate)
  (cond
    ;((member? expression '(TRUE True true)) #t)
    ;((member? expression '(FALSE False false) #f))
    
    ((equal? '< (firstelement  expression))
      (< (booleanevaluate (secondelement expression) cstate) (booleanevaluate (thirdelement expression) cstate)))
    ((equal? '> (firstelement  expression))
      (> (booleanevaluate (secondelement expression) cstate) (booleanevaluate (thirdelement expression) cstate)))
    ((equal? '&& (firstelement expression))
      (and (booleanevaluate (secondelement expression) cstate)))
    ((equal? '|| (firstelement expression))
      (or (booleanevaluate (secondelement expression) cstate) (booleanevaluate (thirdelement expression) cstate)))
    ))

;TODO lookup variables
(define (intevaluate expression cstate)
  (cond
    ((number? expression) expression)
    ((list? (firstelement expression)) (intevaluate (firstelement expression) cstate))
    ((number? (firstelement expression)) (firstelement expression))
    ((equal? '- (firstelement expression))
      (- (intevaluate (secondelement expression) cstate) (intevaluate (thirdelement  expression) cstate)))
    ((equal? '/ (firstelement  expression))
      (floor (/ (intevaluate (secondelement expression) cstate) (intevaluate (thirdelement  expression) cstate))))
    ((equal? '* (firstelement  expression))
      (* (intevaluate (secondelement expression) cstate) (intevaluate (thirdelement expression) cstate)))
    ((equal? '+ (firstelement  expression))
      (+ (intevaluate (secondelement expression) cstate) (intevaluate (thirdelement expression) cstate)))
    ((equal? '% (firstelement  expression))
      (modulo (intevaluate (secondelement expression) cstate) (intevaluate (thirdelement  expression) cstate)))
    (else (undefinedexpressionerror))))


;TODO fix this guy, he doesn't do anything
(define (unaryoperator  expression cstate)
  (cond
    ((equal? 'return (firstelement  expression)) (read (secondelement expression) cstate))
    ((equal? '- (firstelement  expression)) (read (secondelement  expression) cstate))
    ((equal? '! (firstelement  expression)) (read (secondelement  expression) cstate))
    ((equal? 'var (firstelement  expression) (read (secondelement  expression) (declare (secondelement  expression) cstate))))
    (else (undefinedunaryoperator))))
