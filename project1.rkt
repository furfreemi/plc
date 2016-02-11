;Larissa Marcich, Dina Benayad-Cherif
;Project 1: EECS 345


(load "simpleParser.scm")


(define interpret
  (lambda (filename)
    (evalParseTree (parser filename) new_state)
    )
  )


(define new_state '(() ()))
(define operand car)

(define op1
  (lambda (exp state)
    (M_value (cadr exp) state)))

(define op2
  (lambda (exp state)
    (M_value (caddr exp) state)))


(define evalParseTree
  (lambda (tree state)
    (cond
      ((number? tree) tree)
      ((eq? 'var (operator tree)) 0 )
      ((eq? '= (operator tree)) 0)
      (else 0)
      )
    )
  )

;op1: cadr
;op2: caddr
(define add
  (lambda (exp state)
    (cond
      ((eq? (length exp) 1) (append (cons (cons (car exp) (car state)) '()) (cons (cons 'error (cadr state)) '())))
      ((eq? (length exp) 2) (append (cons (cons (car exp) (car state)) '()) (cons (cons (op1 exp state) (cadr state)) '())))
      (else (error 'unknown "unknown expression"))
      )
    )
  )


(define lookup
  (lambda (exp state)
    (cond
      ((null? (car state)) 'error)
      ((eq? (caar state) exp) (caadr state))
      (else (lookup exp (cons (cdar state) (cons (cdadr state) '()))))
      )
    )
  )


(define removevar
  (lambda (exp state)
    (cond
      ((null? (vars state)) state)
      ((eq? exp (firstvar state)) (cons (cdar state) (cons (cdadr state) '())))
      (else (cons (cons (firstvar state) (vars (removevar exp (cons (cdar state) (cons (cdadr state) '()))))) (cons (cons (firstval state) (vals (removevar exp (cons (cdar state) (cons (cdadr state) '()))))) '())))
      )
    )
  )

(define vars car)
(define vals cadr)
(define firstvar caar)
(define firstval caadr)
      

(define M_state
  (lambda (exp state)
    (cond
      ((eq? (operand exp) 'var) (add (cdr exp) state))
      ((eq? (operand exp) '=) (add (cdr exp) (removevar (cadr exp) state)))
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
      ((eq? (operand exp) 'return) (M_value (cdr exp) state))
      )
    )
  )


(define condition cadr)
(define stmt1 caddr)
(define stmt2 cadddr)
    
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((boolean? exp) exp)
      ((not (pair? exp)) (if (eq? (lookup exp state) 'error)(error 'unknown "variable undeclared")  ;unknown variable: error
                         (lookup exp state))) ;if var value exists in state, return value
      ((eq? (operand exp) '+) (+ (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '-) (- (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '*) (* (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '/) (quotient (op1 exp state) (op2 exp state)))
      ((eq? (operand exp) '%) (remainder (op1 exp state) (op2 exp state)))
      ((pair? (operand exp)) (M_value (car exp) state))
      (else (error 'unknown "unknown expression")) 
      )
    )
  )


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


