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
(define new_state '(() ()))

(define vars car)
(define vals cadr)
(define firstvar caar)
(define firstval caadr)

(define condition cadr)
(define stmt1 caddr)
(define stmt2 cadddr)
      
(define operand car)

;operator definitions handle variable lookup
(define op1
  (lambda (exp state)
    (M_value (cadr exp) state)))

(define op2
  (lambda (exp state)
    (M_value (caddr exp) state)))






;eventually: take in entire tree, empty state: break up & call other functions
(define evalParseTree
  (lambda (tree state)
    (cond
      ((eq? (car state) 'FINISH) (cadr state))
      (else (evalParseTree (cdr tree) (M_state (car tree) state)))
    )
  )
  )



; add new variable and value (or initialize to error) to state, return new state
(define add
  (lambda (exp state)
    (cond
      ((eq? (length exp) 1) (append (cons (cons (car exp) (vars state)) '()) (cons (cons 'error (cadr state)) '())))
      ((eq? (length exp) 2) (append (cons (cons (car exp) (vars state)) '()) (cons (cons (op1 exp state) (cadr state)) '())))
      (else (error 'unknown "unknown expression"))
      )
    )
  )


; get value of a variable from state: returns 'error if undefined
(define lookup 
  (lambda (exp state)
    (cond
      ((null? (car state)) 'error)
      ((eq? (caar state) exp) (caadr state))
      (else (lookup exp (cons (cdar state) (cons (cdadr state) '()))))
      )
    )
  )

;checks if state contains a variable (if variable has been initialized yet)
(define initialized
  (lambda (var? varlist)
    (cond
      ((null? varlist) #f)
      ((eq? (car varlist) var?) #t)
      (else (initialized var? (cdr varlist)))
      )
    )
  )
    


; remove variable and its value from state, return new state
; if variable not present just returns existing state
(define removevar
  (lambda (exp state)
    (cond
      ((null? (vars state)) state)
      ((eq? exp (firstvar state)) (cons (cdar state) (cons (cdadr state) '())))
      (else (cons (cons (firstvar state) (vars (removevar exp (cons (cdar state) (cons (cdadr state) '()))))) (cons (cons (firstval state) (vals (removevar exp (cons (cdar state) (cons (cdadr state) '()))))) '())))
      )
    )
  )


;Mstate: takes in expression, passes to Mbool or Mval as necessary to evaluate
(define M_state
  (lambda (exp state)
    (cond
      ((eq? (operand exp) 'var) (add (cdr exp) state))
      ((eq? (operand exp) '=) (if (initialized (operand exp) (vars state))
                                  (add (cons (cadr exp) (cons (M_value (caddr exp) state)'())) (removevar (cadr exp) state))
                                  (error 'unknown "variable not yet declared"))) 
      ((eq? (operand exp) 'if) (if (M_boolean (condition exp) state)
                                   (M_state (stmt1 exp) state)
                                   (if (eq? (length exp) 4)
                                       (M_state (stmt2 exp) state)
                                       state))
                               )
      ((eq? (operand exp) 'while) (if (M_boolean (condition exp) state)
                                      (M_state exp (M_state (stmt1 exp) state))
                                      state
                                      )
                                  )
      ((eq? (operand exp) 'return) (cons 'FINISH (cons (M_value (cdr exp) state) '())))
      )
    )
  )


;takes in expression and attempts to evaluate its value    
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((boolean? exp) exp)
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


