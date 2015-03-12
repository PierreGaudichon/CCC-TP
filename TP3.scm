;; LOOKUP : variable -> (list (pair variable expression)) -> value 
(define LOOKUP (lambda (var set)
  (cond 
    ((null? set) NIL)
    ((equal? (caar set) var) (cdar set))
    (else (LOOKUP var (cdr set))))))

                      
;; ASSIGN : variable -> expression -> (list (pair variable expression)) 
;;                     -> (list (pair variable expression))
(define ASSIGN (lambda (var val set)
  ;(display "ASSIGN")
  ;(display var)
  ;(display val)
  ;(display "\n")
  (cond 
    ((null? set) (list (cons var val)))
    ((equal? (caar set) var) (cons (cons var val) (cdr set)))
    (else (cons (car set) (ASSIGN var val (cdr set)))))))



;; semantics of the expressions
;; EVAL : expression -> (list (pair variable expression)) 
;;                   -> expression
(define EVAL (lambda (e set)
  (cond
    ((EQ? e) (if (equal? (EVAL (EQ->arg1 e) set) (EVAL (EQ->arg2 e) set))
      (CONS NIL NIL)
      NIL))
    ((VAR? e) (LOOKUP e set))
    ((CONS? e) (CONS (EVAL (CONS->arg1 e) set) (EVAL (CONS->arg2 e) set)))
    ((CST? e) e)
    ((HD? e) (if (CONS? (HD->arg e)) 
      (EVAL (CONS->arg1 (HD->arg e)) set)
      NIL))
    ((TL? e) (if (CONS? (TL->arg e)) 
      (EVAL (CONS->arg2 (TL->arg e)) set)
      NIL))
    ((NIL? e) NIL))))

;;
;;
(define STEP (lambda (c set)
  (cond 
    ((SET? c) (ASSIGN (SET->var c) (EVAL (SET->expr c) set) set))
    ((NOP? c) set)
    ((WHILE? c) (if (NIL? (EVAL (WHILE->cond c) set))
      ; (STEP* (WHILE->body c) set) ; ATTENTION, on evalue le set une fois de plus
      set ; Normalement juste cette ligne et pas la precedente.
      (STEP c (STEP* (WHILE->body c) set)))
    ))))
    
;;
;;
(define STEP* (lambda (cs set)
  (if (null? cs)
    set
    (STEP* (cdr cs) (STEP (car cs) set)))))

;;
;;
(define STORE-set (lambda (vars vals)
  (if (null? vars)
    '()
    (ASSIGN (car vars) (car vals) (STORE-set (cdr vars) (cdr vals))))))

;;
;;
(define STORE-get (lambda (vars set)
  (if (null? vars)
    '()
    (cons (EVAL (car vars) set) (STORE-get (cdr vars) set)))))


;;
;;
(define EXEC (lambda (p vars)
  (STORE-get (PROGR->out p) (STEP* (PROGR->body p) (STORE-set (PROGR->in p) vars)))))


(load "test-interpreter.ss")
