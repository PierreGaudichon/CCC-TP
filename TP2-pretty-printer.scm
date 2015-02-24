;; TP 2
;; Pierre Gaudichon & Etnocel

(load "test.ss")
(load "ccc.ss")

(define show-all-tests #f)


;; default-indent :: Int
;;                ::  a
;; Default indentation for output programm. `a` is the number of space.
(define indent-default 1)


;; indent-search :: context-name x [indent-spec] ->  Integer
;; Searches a list of indentation specifications.
(define indents-search (lambda (context-name indents-spec)
  (let (
        (pair (assoc context-name indents-spec))) 
    (if pair 
        (cdr pair)
        indent-default))))


;; make-indent :: Int -> String
;;                 a  -> String (length = a) composed of whitespace
(define make-indent (lambda (a)
 (make-string a #\space)))


;; append-string-before-all :: String x [String] -> [String]
;;                                b   ,    s     -> Append b before each string from l.
(define append-string-before-all (lambda (b l)
  (map 
    (lambda (s)
      (string-append b s))
    l)))


;; append-string-after-all :: String x [String] -> [String]
;;                                b   ,    s     -> Append b after each string from l.
(define append-string-after-all (lambda (b l)
  (map 
    (lambda (s)
      (string-append s b))
    l)))


;; append-string-after-all-butt-the-last-bitch :: String x [String] -> [String]
;;                                                  b    ,   l      -> Sefl-explanatory.
(define append-string-after-all-butt-the-last-bitch (lambda (b l)
  (if (<= (length l) 1)
    l
    (cons (string-append (car l) b) (append-string-after-all-butt-the-last-bitch b (cdr l))))))


;; pretty-print-expr :: expr x [indent-spec] -> String
;;                       e   ,      s        -> 
(define pretty-print-expr (lambda (e s) (cond
  ((NIL? e)  "nil")
  ((CST? e)  (CST->name e))
  ((VAR? e)  (VAR->name e))
  ((CONS? e) (string-append "(cons " (pretty-print-expr (CONS->arg1 e) s) " " (pretty-print-expr(CONS->arg2 e) s) ")"))
  ((HD?  e)  (string-append "(hd " (pretty-print-expr (HD->arg e) s) ")"))
  ((TL? e)   (string-append "(tl " (pretty-print-expr (TL->arg e) s) ")"))
  ((EQ? e)   (string-append (pretty-print-expr (EQ->arg1 e) s) " =? " (pretty-print-expr (EQ->arg2 e) s))))))


;; pretty-print-command :: command x [indent-spec] -> [String]
;;                          c      ,       s       ->
(define pretty-print-command (lambda (c s)
  (cond
    ((NOP? c) (list "nop"))
    ((SET? c) (list (string-append (VAR->name (SET->var c)) " := " (pretty-print-expr (SET->expr c) s))))
    ((WHILE? c) (append
      (list (string-append "while " (pretty-print-expr (WHILE->cond c) s) " do"))
      (append-string-before-all (make-indent (indents-search "WHILE" s)) (pretty-print-commands (WHILE->body c) s))
      (list"od")))
    ((FOR? c) (append
      (list (string-append "for " (pretty-print-expr (FOR->count c) s) " do"))
      (append-string-before-all (make-indent (indents-search "FOR" s)) (pretty-print-commands (FOR->body c) s))
      (list "od")))
    ((IF? c) (append
      (list (string-append "if " (pretty-print-expr (IF->cond c) s) " then"))
      (append-string-before-all (make-indent (indents-search "IF" s)) (pretty-print-commands (IF->then c) s))
      (list "else")
      (append-string-before-all (make-indent (indents-search "IF" s)) (pretty-print-commands (IF->else c) s))
      (list "fi"))))))


;; pretty-print-commands :: [command] x [indent-spec] -> [String]
;;                             cs     ,     s         ->
(define pretty-print-commands (lambda (cs s)
  (cond
    ((null? cs)
      (list))
    ((= (length cs) 1)
      (pretty-print-command (car cs) s))
    (else (let* ( 
      (command (pretty-print-command (car cs) s))
      (commands (pretty-print-commands (cdr cs) s))
      (dnammoc (reverse command))
      (command-ready (reverse (cons (string-append (car dnammoc) " ;") (cdr dnammoc)))))
        (append command-ready commands))))))
      

;; pretty-print-in :: [Var] x [indent-spec] -> String
(define pretty-print-in (lambda (vs s)
  (cond 
    ((null? vs) "")
    ((= (length vs) 1) (VAR->name (car vs)))
    (else (string-append (VAR->name (car vs)) ", " (pretty-print-in (cdr vs) s))))))

(define pretty-print-out pretty-print-in)


;; pretty-print-progr :: Progr x [indent-spec] -> [String]
(define pretty-print-progr (lambda (progr s)
  (append
   (list (string-append "read " (pretty-print-in (PROGR->in progr) s)) "%")
   (append-string-before-all (make-indent (indents-search "PROGR" s)) (pretty-print-commands (PROGR->body progr) s))
   (list "%" (string-append "write " (pretty-print-out (PROGR->out progr) s))))))


;; pretty-print :: Progr x [indent-spec] -> String
(define pretty-printer (lambda (progr . s)
  (apply string-append (append-string-after-all-butt-the-last-bitch "\n" (pretty-print-progr progr s)))))