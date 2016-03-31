;Larissa Marcich, Dina Benayad-Cherif, Adam Gleichsner
;lmm154, dxb448, amg188
;Project 3: EECS 345


(load "functionParser.scm")

;required interpret method
(define interpret
  (lambda (filename)
    (returnval (evalParseTree (parser filename) new_state))))

(define value car)
(define returnval
  (lambda (pair)
    (cond
      ((eq? (value pair) #t) 'true)
      ((eq? (value pair) #f) 'false)
      (else (value pair)))))

;base abstractions 
(define new_state '((() ())))
(define new_layer '(()()))

(define first_exp car)
(define remaining_exp cdr)

;updated layer state abstractions
(define toplayer_vars caar)
(define toplayer_vals cadar)

; add var and val to top layer of state
(define addtotop
  (lambda (var val state)
    (cons (cons (cons var (toplayer_vars state)) (cons (cons val (toplayer_vals state)) '())) (cdr state))))

(define condition cadr)
(define stmt1 caddr)
(define stmt2 cadddr)

(define varname car)
(define operand car)

;Helper methods and abstractions for try-catch-finally
(define finally_body cadddr)
(define catch_body caddr)
(define catch_var caadr)
(define try_body cadr)

;operator definitions handle variable lookup
(define op1
  (lambda (exp state throw)
    (get_value (M_value (cadr exp) state throw))))

(define op2
  (lambda (exp state throw)
    (get_value (M_value (caddr exp) state throw))))

(define invalid_throw (lambda (s) (error 'error "Throw without catch")))
(define invalid_break (lambda (s) (error 'error "Illegal break")))
(define invalid_continue (lambda (s) (error 'error "Illegal continue")))

(define cadddar
  (lambda (l)
    (car (cdr (cdr (cdr (car l)))))))

;take in entire tree, empty state: break up & call other functions
(define evalParseTree
  (lambda (tree state)        
    (call/cc
     (lambda (return)
      (cond
        ((null? tree) (error 'error "No return")) ;IF END: RETURNS STATE- for testing purposes only, remove! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((eq? (cadar tree) 'main) (evalParseTree (remaining_exp (cadddar tree)) (M_state (first_exp (cadddar tree)) (add_scope state) invalid_break invalid_continue return invalid_throw)))
        (else (evalParseTree (remaining_exp tree) (M_state (first_exp tree) state invalid_break invalid_continue return invalid_throw)))
    )))))
  
  


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
  (lambda (exp state throw)
    (cond
      ;((initialized? (varname exp) state) (error 'unknown "redefining already declared variable")) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;alter to take in new state type?
      ((eq? (length exp) 1) ; var not immediately initialized: add in with 'error inital value
           (addtotop (varname exp) 'error state))
      ((eq? (length exp) 2) ; add in var with initalized value
           (addtotop (varname exp) (op1 exp state throw) state))
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
      ((not (list? (car state))) (car state))
      ((null? (toplayer_vars state)) (lookup exp (cdr state)))
      ((eq? (first_topvar state) exp) (first_topval state))
      (else (lookup exp (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state))))
     )
    )
  )
(define global_state
  (lambda (state)
    (if (null? (cdr state))
        (car state)
        (global_state (cdr state)))))

(define top_state
  (lambda (state)
    (car state)))

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
      ((not (list? (car state))) state)
      ((null? (toplayer_vars state)) (pushlayer (change_value exp value (lower_layers state))))
      ((eq? (lookup exp state) 'UNDECLARED) state)
      ((eq? exp (first_topvar state)) (build_modified_state (toplayer_vars state) (cons value (remaining_topvals state)) (lower_layers state)))
      (else (addtotop (first_topvar state) (first_topval state) (change_value exp value (build_modified_state (remaining_topvars state) (remaining_topvals state) (lower_layers state)))
                      )))))

(define begin_helper
  (lambda (exp state break continue return throw)
    ;(let ((scope (lambda (break2 continue2)
                   ;(cdr (state_run_helper exp (cons (cons '() (cons '() '())) state) break2 continue2 throw return)))))
    ;(scope (lambda (s) (break (cdr s))) (lambda (s) (continue (cdr s)))))))
    (cond
      ((null? exp) state)
      (else (begin_helper (remaining_exp exp) (M_state (first_exp exp) state break continue return throw) break continue return throw)))))

(define add_scope
  (lambda (state)
    (cons (cons '() (cons '() '())) state)))

;helper method for throw, pushes a state for throw at the end of the state
(define push_to_end
  (lambda (value state)
    (append state (cons (cons '() (cons (cons value '()) '())) '()))))

;helper method for attachvariable
(define addstate
  (lambda (vars values state)
    (cons (cons vars (cons values '())) state)))

;helper method that attaches the variable from catch to the value thrown in the state
(define attach_variable
(lambda (var state)
    (cond
      ((null? state) state)
      ;((null? (toplayer_vars state)) (attach_variable var (lower_layers state))))
      ((and (eq? '() (toplayer_vars state)) (not (null? (toplayer_vals state)))) (build_modified_state (cons var (toplayer_vars state)) (toplayer_vals state) (lower_layers state)))
      ((eq? '() (toplayer_vars state)) (cons (car state) (attach_variable var (poplayer state))))
      (else (addstate (toplayer_vars state) (toplayer_vals state) (attach_variable var (build_modified_state (toplayer_vars (cdr state)) (toplayer_vals (cdr state)) (lower_layers (cdr state))))
                      )))))


;Mstate: takes in expression, passes to Mbool or Mval as necessary to evaluate
(define M_state
  (lambda (exp state break continue return throw)
    (letrec ((M_stateloop (lambda (exp state break continue return throw)
      (cond
        ((eq? (operand exp) 'var) (add (remaining_exp exp) state throw))
        ((eq? (operand exp) 'begin) (begin_helper (remaining_exp exp) state break continue return throw))
        ((eq? (operand exp) '=) (if (and (declared? (cadr exp) state) (not (eq? (op2 exp state throw) 'UNDECLARED)))
                                    (change_value (cadr exp) (op2 exp state throw) state)
                                    (error 'unknown "variable not yet declared")))
        ((eq? (operand exp) 'if) (if (M_boolean (condition exp) state throw)
                                   (M_stateloop (stmt1 exp) state break continue return throw)
                                   (if (eq? (length exp) 4)
                                       (M_stateloop (stmt2 exp) state break continue return throw)
                                        state)))
        ((eq? (operand exp) 'while) (M_state_while exp state break continue return throw))
        ((eq? (operand exp) 'try) (M_state_try-catch-finally exp state break continue return throw))
        ((eq? (operand exp) 'break) (if (eq? break 'error)
                                      (error 'unknown "cannot execute break outside of block")
                                      (break (stripstate state))))
        ((eq? (operand exp) 'continue) (if (eq? continue 'error)
                                      (error 'unknown "cannot execute continue outside of block")
                                      (continue (stripstate state))))
        ((eq? (operand exp) 'throw) (throw (push_to_end (get_value (M_value (remaining_exp exp) state throw)) state)))
        ((eq? (operand exp) 'function) (M_state_function exp state break continue return throw))
        ((eq? (operand exp) 'funcall) (M_state_funcall exp state break continue return throw))
        ((eq? (operand exp) 'return) (return (M_value (remaining_exp exp) state throw))))))) (M_stateloop exp state break continue return throw))))

(define M_state_funcall
  (lambda (exp state break continue return throw)
       ;(cons (cadr (state_run_helper (cadr (lookup (cadr exp) state)) (cons (bind_func_vars (car (lookup (cadr exp) state)) (cddr exp) (cons '() (cons '() '())) (make_static_state state)) (cons (global_state state) '()))  break continue (lambda (v) v) throw)) '())))
    (cadr (state_run_helper (cadr (lookup (cadr exp) state)) (cons (bind_func_vars (car (lookup (cadr exp) state)) (cddr exp) (cons '() (cons '() '())) (make_static_state state) throw) (cons (global_state state) '()))  break continue (lambda (v) (cadr v)) throw))))

(define M_state_function
  (lambda (exp state break continue return throw)
    (addtotop (cadr exp) (cons (caddr exp) (cons (cadddr exp) '())) state)))
    
    
;(catch (e) (()()))
(define catch_exp caddr)
(define finally_exp cadr)

(define state_run_helper
  (lambda (exp state break continue return throw)
    (call/cc
     (lambda (return)
       (letrec ((loop (lambda (exp2 state2)
                     (if (null? exp2)
                         (cons '() (cons state2 '()))
                         (loop (remaining_exp exp2) (M_state (first_exp exp2) state2 break continue return throw)))))) (loop exp state))))))

(define run_try state_run_helper)
(define run_finally state_run_helper)
(define run_catch state_run_helper)

(define M_state_while
  (lambda (exp state break continue return throw)
    (call/cc
     (lambda (break_while)
       (letrec ((loop (lambda (exp2 state2)
                        (if (M_boolean (condition exp2) state2 throw)
                            (loop exp2 (M_state (stmt1 exp2) state2 break (lambda (state3) (break_while (loop exp2 state3))) return throw))
                            state2))))
         (loop exp state))))))

(define M_state_try-catch-finally
  (lambda (exp state break continue return throw)
    (call/cc
     (lambda (throw_break)
        (letrec ((finally (lambda (s)
                            (if (null? (finally_body exp))
                                (cadr s)
                                (cadr (run_finally (finally_exp (finally_body exp)) (cadr s) break continue return throw_break)))))
                 (catch (lambda (s)
                          (finally (cons '() (cons (removevar (catch_var (catch_exp exp)) (cadr (run_catch (catch_exp (catch_body exp)) (attach_variable (catch_var (catch_body exp)) s) break continue return invalid_throw))) '())))))
                 (try (lambda (s catch_throw)
                        (finally (run_try (try_body exp) s break continue return catch_throw)))))
          (try state (lambda (s) (throw_break (catch s)))))))))

;takes in expression and attempts to evaluate its value
(define popcatchvar
  (lambda (v s)
    (removevar v s)))

(define add_state
  (lambda (v state)
    (cons v (cons state '()))))

(define get_value car)

(define M_value
  (lambda (exp state throw)
    (cond
      ((number? exp) (add_state exp state))
      ((boolean? exp) (add_state exp state))
      ((eq? exp 'true) (add_state #t state))
      ((eq? exp 'false) (add_state #f state))
      ((bool? exp) (add_state (M_boolean exp state throw) state))
      ((not (pair? exp)) (if (eq? (lookup exp state) 'error)
                             (error 'unknown "var unknown")  ;unknown variable: error
                             (add_state (lookup exp state) state))) ;if var value exists in state, return value
      ((eq? (operand exp) '+) (add_state (+ (op1 exp state throw) (op2 exp state throw)) state))
      ((eq? (operand exp) '-) (if (eq? (length exp) 3)
                                  (add_state (- (op1 exp state throw) (op2 exp state throw)) state)
                                  (add_state (- 0 (op1 exp state)) state)))
      ((eq? (operand exp) '*) (add_state (* (op1 exp state throw) (op2 exp state throw)) state))
      ((eq? (operand exp) '/) (add_state (quotient (op1 exp state throw) (op2 exp state throw)) state))
      ((eq? (operand exp) '%) (add_state (remainder (op1 exp state throw) (op2 exp state throw)) state))
      ((eq? (operand exp) 'funcall) (state_run_helper (cadr (lookup (cadr exp) state)) (cons (bind_func_vars (car (lookup (cadr exp) state)) (cddr exp) (cons '() (cons '() '())) (make_static_state state) throw) (cons (global_state state) '()))  invalid_break invalid_continue (lambda (v) (return v)) throw))
      ((pair? exp) (M_value (car exp) state throw))
      (else (error 'unknown exp)) 
      )
    )
  )

(define make_static_state
  (lambda (state)
    (cons (global_state state) (cons (top_state state) '()))))

(define bind_func_vars
  (lambda (vars vals new_scope state throw)
    (cond
      ((and (null? vars) (null? vals)) new_scope)
      ((and (null? vars) (not (null? vals))) (error 'error "Too many input values"))
      ((and (not (null? vars)) (null? vals)) (error 'error "Not enough input values"))
      (else (bind_func_vars (cdr vars) (cdr vals) (cons (cons (car vars) (car new_scope)) (cons (cons (get_value (M_value (car vals) state throw)) (cadr new_scope)) '())) state throw)))))

; evaluates boolean expressions
(define M_boolean
  (lambda (exp state throw)
    (cond
      ((boolean? exp) exp)
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((eq? (operand exp) '!=) (not (eq? (op1 exp state throw) (op2 exp state throw))))
      ((eq? (operand exp) '==) (eq? (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '>) (> (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '<) (< (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '<=) (<= (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '>=) (>= (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '&&) (and (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '||) (or (op1 exp state throw) (op2 exp state throw)))
      ((eq? (operand exp) '!) (not (op1 exp state throw)))
      (else (error 'unknown "unknown expression"))
      )
    )
  )