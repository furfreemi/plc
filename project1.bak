;Larissa Marcich, Dina Benayad-Cherif, Adam Gleichsner
;lmm154, dxb448, amg188
;Project 1: EECS 345


(load "simpleParser.scm")

;required interpret method
(define interpret
  (lambda (filename)
    (evalParseTree (parser filename) new_state)
    )
  )



;base abstractions 
(define new_state '((() ())))
(define new_layer '(()()))
(define returnval cadr)

(define first_exp car)
(define remaining_exp cdr)

;updated layer state abstractions
(define toplayer_vars caar)
(define toplayer_vals cadar)

; add var and val to top layer of state
(define addtotop
  (lambda (var val state)
    (cons (cons (cons var (toplayer_vars state)) (cons (cons val (toplayer_vals state)) '())) (cdr state))
    ))


(define condition cadr)
(define stmt1 caddr)
(define stmt2 cadddr)

(define varname car)
(define operand car)

;operator definitions handle variable lookup
(define op1
  (lambda (exp state)
    (M_value (cadr exp) state)))

(define op2
  (lambda (exp state)
    (M_value (caddr exp) state)))




;take in entire tree, empty state: break up & call other functions
(define evalParseTree
  (lambda (tree state)
    (cond
      ((null? tree) state) ;IF END: RETURNS STATE- for testing purposes only, remove! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;((eq? (car state) 'FINISH) (cond
                                   ;((eq? #f (returnval state)) 'false)
                                   ;((eq? #t (returnval state)) 'true)
                                   ;(else (returnval state))))
      (else (evalParseTree (remaining_exp tree) (M_state (first_exp tree) state 'error 'error)))
    )
  )
  )


;helper: is this a boolean expression?
(define bool?
  (lambda (exp)
    (if (pair? exp)
        (or (or (or (or (or (or (or (or
            (eq? (operand exp) '!=)
            (eq? (operand exp) '==))
            (eq? (operand exp) '>))
            (eq? (operand exp) '<))
            (eq? (operand exp) '<=))
            (eq? (operand exp) '>=))
            (eq? (operand exp) '&&))
            (eq? (operand exp) '||))
            (eq? (operand exp) '!))
        #f)))

; add new empty layer to state
(define pushlayer
  (lambda (state)
    (cons new_layer state)))

; remove top layer from state
(define poplayer
  (lambda (state)
    (cdr state)))

; add new variable and value (or initialize to error) to top layer of state, return new state
(define add
  (lambda (exp state)
    (cond
      ((initialized? (varname exp) state) (error 'unknown "redefining already declared variable")) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;alter to take in new state type?
      ((eq? (length exp) 1) ; var not immediately initialized: add in with 'error inital value
           (addtotop (varname exp) 'error state))
      ((eq? (length exp) 2) ; add in var with initalized value
           (addtotop (varname exp) (op1 exp state) state))
      (else (error 'unknown "unknown expression"))
      )
    )
  )

; very first variable in top layer of state
(define first_topvar (lambda (state) (car (toplayer_vars state))))

; very first value in top layer of state
(define first_topval (lambda (state) (car (toplayer_vals state))))

; state sans the very first variable in the very top layer
(define remaining_topvars
  (lambda (state)
    (cdr (toplayer_vars state))))     

; state sans the very first value in the very top layer
(define remaining_topvals
  (lambda (state)
    (cdr (toplayer_vals state))))

; remove top layer from state
(define lower_layers cdr)

; creates a new state version with topvars and topvals making up the first layer, state making up the rest of the lower layers
(define build_modified_state
  (lambda (topvars topvals state)
    (cons (cons topvars (cons topvals '())) state)))


; get value of a variable from state: returns 'error if undefined
(define lookup 
  (lambda (exp state)
    (cond
      ((null? state) 'UNDECLARED)  ;PREVIOUSLY RETURNED 'error: MAKE SURE ALL DEPENDENCIES ARE UPDATED
      ((null? (toplayer_vars state)) (lookup exp (cdr state)))
      ((eq? (first_topvar state) exp) (first_topval state))
      (else (lookup exp (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state))))
      )
    )
  )

;checks if state contains a variable (if variable has been initialized yet) ;PREVIOUSLY TOOK VARLIST: make sure all uses are updated to input entire state
(define initialized?
  (lambda (exp state)
    (cond
      ((eq? (lookup exp state) 'UNDECLARED) #f) ;not even declared yet
      ((eq? (lookup exp state) 'error) #f)    ;simply uninitialized
      (else #t)
      )
    )
  )

;checks to see if variable has been declared yet
(define declared?
  (lambda (exp state)
    (cond
      ((eq? (lookup exp state) 'UNDECLARED) #f)    ;simply uninitialized
      (else #t)
      )
    )
  )
    


; remove variable and its value from state, return new state
; if variable not present just returns existing state
(define removevar
  (lambda (exp state)
    (cond
      ((null? state) state)
      ((null? (toplayer_vars state)) (pushlayer (removevar exp (lower_layers state))))
      ((eq? (lookup exp state) 'UNDECLARED) state)
      ((eq? exp (first_topvar state)) (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state)))
      (else (addtotop (first_topvar state) (first_topval state) (removevar exp (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state)))))
      )
    )
  )

(define stripstate
  (lambda (state)
    (cdr state)))

(define change_value
  (lambda (exp value state)
    (cond
      ((null? state) state)
      ((null? (toplayer_vars state)) (pushlayer (change_value exp value (lower_layers state))))
      ((eq? (lookup exp state) 'UNDECLARED) state)
      ((eq? exp (first_topvar state)) (build_modified_state (toplayer_vars state) (cons value (remaining_topvals state)) (lower_layers state)))
      (else (addtotop (first_topvar state) (first_topval state) (change_value exp value (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state)))
                      )))))

(define begin_helper
  (lambda (exp state break continue)
    (cond
      ((null? exp) (poplayer state))
      (else (begin_helper (remaining_exp exp) (M_state (first_exp exp) state break continue) break continue)))))


;Mstate: takes in expression, passes to Mbool or Mval as necessary to evaluate
(define M_state
  (lambda (exp state break continue)
    (call/cc
     (lambda (return)
       (letrec ((M_stateloop (lambda (exp state break continue)
    (cond
      ((eq? (operand exp) 'var) (add (remaining_exp exp) state))
      ((eq? (operand exp) 'begin) (begin_helper (remaining_exp exp) (cons '(() ()) state) break continue))
      ((eq? (operand exp) '=) (if (and (declared? (cadr exp) state) (declared? (op2 exp state) state))
                                  (change_value (cadr exp) (op2 exp state) state)
                                  (error 'unknown "variable not yet declared")))
      ((eq? (operand exp) 'if) (if (M_boolean (condition exp) state)
                                   (M_stateloop (stmt1 exp) state break continue)
                                   (if (eq? (length exp) 4)
                                       (M_stateloop (stmt2 exp) state break continue)
                                        state))
                               )
      ((eq? (operand exp) 'while) (call/cc
                                   (lambda (break)
                                   (letrec ((loop (lambda (exp2 state2)  
                                   (if (M_boolean (condition exp2) state2)
                                      (loop exp2
                                       (call/cc
                                        (lambda (continue)
                                       (M_stateloop (stmt1 exp2) state2 break continue))))
                                      state2
                                      )
                                  ))) (loop exp state)))))
      ((eq? (operand exp) 'break) (break (stripstate state)))
      ((eq? (operand exp) 'continue) (continue (stripstate state)))
      ((eq? (operand exp) 'return) (return (M_value (remaining_exp exp) state))))))) (M_stateloop exp state break continue))))))



;takes in expression and attempts to evaluate its value    
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((boolean? exp) exp)
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((bool? exp) (M_boolean exp state))
      ((not (pair? exp)) (if (eq? (lookup exp state) 'error)
                             (error 'unknown "var unknown")  ;unknown variable: error
                             (lookup exp state))) ;if var value exists in state, return value
      ((eq? (operand exp) '+) (+ (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '-) (if (eq? (length exp) 3)
                                  (- (op1 exp state) (op2 exp state))
                                  (- 0 (op1 exp state))))
      ((eq? (operand exp) '*) (* (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '/) (quotient (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '%) (remainder (op1 exp state) (op2 exp state)))
      ((pair? exp) (M_value (car exp) state))
      (else (error 'unknown exp)) 
      )
    )
  )

; evaluates boolean expressions
(define M_boolean
  (lambda (exp state)
    (cond
      ((boolean? exp) exp)
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((eq? (operand exp) '!=) (not (eq? (op1 exp state) (op2 exp state))))
      ((eq? (operand exp) '==) (eq? (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '>) (> (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '<) (< (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '<=) (<= (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '>=) (>= (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '&&) (and (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '||) (or (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '!) (not (op1 exp state)))
      (else (error 'unknown "unknown expression"))
      )
    )
  )