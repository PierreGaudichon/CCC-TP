
;;------------------- abstract syntax of programs ------------------------

;; PROGR : (list identifier) -> (list instruction) -> (list identifier) -> program

(define (PROGR in body out) (list 'ccc-progr in body out))
(define (PROGR? expr) (equal? (car expr) 'ccc-progr))
(define (PROGR->in progr) (cadr progr))
(define (PROGR->body progr) (caddr progr))
(define (PROGR->out progr) (car (cdddr progr)))

(define test-progr (PROGR (list 1 2) (list 3 4) (list 5 6)))

;(display "ccc")

;(test (PROGR->in test-progr) (list 1 2))
;(test (PROGR->out test-progr) (list 5 6))
;(test (PROGR->body test-progr) (list 3 4))

;; every instruction or expression is represented as a list 
;; whose first element is its type
;; type of instruction belongs to {WHILE, FOR, SET, IF, NOP, CASE, DO}
;; type of expression belongs to 
;;         {ID,VAR,DATA,INT,EQ,NEQ,CONS,NIL,HD,TL,STR,CONC,NIL,TRUE,FALSE}
(define (ANY-TYPE expr) (car expr))
(define (EXPR-TYPE expr) (car expr))
(define (INSTR-TYPE instr) (car instr))
(define type-WHILE 'ccc-while)
(define type-FOR 'ccc-for)
(define type-SET 'ccc-set)
(define type-IF 'ccc-if)
(define type-NOP 'ccc-nop)
(define type-VAR 'ccc-var)
(define type-CST 'ccc-cst)
(define type-EQ 'ccc-eq)
(define type-CONS 'ccc-cons)
(define type-NIL 'ccc-nil)
(define type-HD 'ccc-hd)
(define type-TL 'ccc-tl)
(define type-ERROR 'ccc-error)
(define (WHILE? expr) (equal? (INSTR-TYPE expr) type-WHILE))
(define (FOR? expr) (equal? (INSTR-TYPE expr) type-FOR))
(define (SET? expr) (equal? (INSTR-TYPE expr) type-SET))
(define (IF? expr) (equal? (INSTR-TYPE expr) type-IF))
(define (NOP? expr) (equal? (INSTR-TYPE expr) type-NOP))
(define (VAR? expr) (equal? (EXPR-TYPE expr) type-VAR))
(define (CST? expr) (equal? (EXPR-TYPE expr) type-CST))
(define (EQ? expr) (equal? (EXPR-TYPE expr) type-EQ))
(define (CONS? expr) (equal? (EXPR-TYPE expr) type-CONS))
(define (NIL? expr) (equal? (EXPR-TYPE expr) type-NIL))
(define (HD? expr) (equal? (EXPR-TYPE expr) type-HD))
(define (TL? expr) (equal? (EXPR-TYPE expr) type-TL))
(define (EXPR? expr) (or (VAR? expr) (CST? expr) 
                         (EQ? expr) (CONS? expr) (NIL? expr) 
                         (HD? expr) (TL? expr) ))
(define (COMM? expr) (or (NOP? expr) (SET? expr) (IF? expr) (FOR? expr) (WHILE? expr)))

;; WHILE : condition -> (list instruction) -> instruction
(define (WHILE cond body) (list type-WHILE cond body))
(define (WHILE->cond while) (cadr while))
(define (WHILE->body while) (caddr while))

;; FOR : count -> (list instruction) -> instruction
(define (FOR count body) (list type-FOR count body))
(define (FOR->count loop) (cadr loop))
(define (FOR->body loop) (caddr loop))

;; IF : condition -> (list instruction) -> (list instruction) -> instruction
(define (IF cond then else) (list type-IF cond then else))
(define (IF->cond cond) (cadr cond))
(define (IF->then cond) (caddr cond))
(define (IF->else cond) (cadddr cond))

;; NOP : instruction
(define NOP (list type-NOP))

;; SET : variable -> expression -> instruction
(define (SET var expr) (list type-SET var expr))
(define (SET->var set) (cadr set))
(define (SET->expr set) (caddr set))

;; VAR : string -> variable
(define (VAR name) (list type-VAR name))
(define (VAR->name var) (cadr var))

;; CST : string -> expression
(define (CST name) (list type-CST name))
(define (CST->name cst) (cadr cst))

;; CONS : expression -> expression -> expression
(define (CONS car cdr) (list type-CONS car cdr))
(define (CONS->arg1 cons) (cadr cons))
(define (CONS->arg2 cons) (caddr cons))

;; HD : expression -> expression
(define (HD cons) (list type-HD cons))
(define (HD->arg head) (cadr head))

;; TL : expression -> expression
(define (TL cons) (list type-TL cons))
(define (TL->arg tail) (cadr tail))

;; EQ : expression -> expression -> condition
(define (EQ expr1 expr2) (list type-EQ expr1 expr2))
(define (EQ->arg1 eq) (cadr eq))
(define (EQ->arg2 eq) (caddr eq))

;; NIL : expression
(define NIL (list type-NIL))

;;------------------------------------------------------

;;------ error handling -----------

;; ERROR : string -> error-report (define type-ERROR 'ccc-error)
(define (ERROR message) (list type-ERROR message))
(define (ERROR->message error) (cadr error))
(define (ERROR? expr) (and (pair? expr) 
                           (equal? (EXPR-TYPE expr) type-ERROR)))

