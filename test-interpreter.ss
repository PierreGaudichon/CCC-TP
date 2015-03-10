(load "test.ss")
(load "ccc.ss")

;; LOOKUP : variable -> (list (pair variable expression)) -> value 

;; ASSIGN : variable -> expression -> (list (pair variable expression)) 
;;                     -> (list (pair variable expression)) 

(display "ASSIGN ")
(test (ASSIGN (VAR "truc") (CST "truc") 
              (ASSIGN (VAR "muche") (CST "mmmmmuche") ())) 
      (list (cons (VAR "muche") (CST "mmmmmuche")) 
            (cons (VAR "truc") (CST "truc")) ))
(test (ASSIGN (VAR "muche") (CST "muche") 
              (ASSIGN (VAR "truc") (CST "truc") 
                      (ASSIGN (VAR "muche") (CST "mmmmmuche") ()))) 
      (list (cons (VAR "muche") (CST "muche")) 
            (cons (VAR "truc") (CST "truc")) ))
(test (ASSIGN (VAR "truc") (CST "trrrrruc") 
              (ASSIGN (VAR "muche") (CST "muche") 
                      (ASSIGN (VAR "truc") (CST "truc") 
                              (ASSIGN (VAR "muche") (CST "mmmmmuche") ())))) 
      (list (cons (VAR "muche") (CST "muche")) 
            (cons (VAR "truc") (CST "trrrrruc")) ))

(define trucmuche (ASSIGN (VAR "muche") (CST "muche") 
                          (ASSIGN (VAR "truc") (CST "truc") 
                                  (ASSIGN (VAR "muche") (CST "mmmmmuche") 
                                          ()))))
;; semantics of the expressions
;; EVAL : expression -> (list (pair variable expression)) 
;;                   -> expression

(display "EVAL ")
(test (EVAL (EQ (EQ NIL NIL) NIL) trucmuche) NIL)
(test (EVAL (EQ (EQ (CST "truc") NIL) NIL) trucmuche) (CONS NIL NIL))
(test (EVAL (HD (CONS (VAR "truc") (VAR "muche"))) trucmuche) 
      (CST "truc"))

(test (EVAL (TL (CONS (VAR "truc") (VAR "muche"))) trucmuche) 
      (CST "muche"))
(test (EVAL (EQ (VAR "truc") (HD (CONS (VAR "truc") (CST "muche")) )) trucmuche) 
      (CONS NIL NIL))
(test (EVAL (EQ (VAR "truc") 
                (HD (CONS (VAR "muche") 
                          (CST "muche")) )) trucmuche)
      NIL)

;; semantics of the instructions
;; STEP : instruction -> (list (pair variable expression)) 
;;                    -> (list (pair variable expression))

;; semantics of lists of instructions
;; STEP* : (list instruction) -> (list (pair variable expression)) 
;;                            -> (list (pair variable expression)) 

(display "STEP ")
(test (STEP (SET (VAR "truc") (CST "a")) 
            trucmuche) 
      (list (cons (VAR "muche") (CST "muche")) (cons (VAR "truc") (CST "a"))))
(test (STEP NOP trucmuche) trucmuche)
(test (EVAL (EQ (VAR "X") NIL) 
            (list (cons (VAR "X") NIL))) (CONS NIL NIL))
(test (STEP (SET (VAR "X") (CST "data")) 
            (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") (CST "data"))))
(test (EVAL (EQ (VAR "X") NIL) 
            (list (cons (VAR "X") (CST "data")))) NIL)
(test (STEP (WHILE (EQ (VAR "X") NIL) (list (SET (VAR "X") (CST "data")))) 
            (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") (CST "data"))))

(display "STEP* ")
(test (STEP* (list (SET (VAR "truc") (CST "a"))) 
             trucmuche) 
      (list (cons (VAR "muche") (CST "muche")) (cons (VAR "truc") (CST "a"))))
(test (STEP* (list (SET (VAR "truc") (CST "a")) (SET (VAR "truc") (CST "a"))) 
             trucmuche) 
      (list (cons (VAR "muche") (CST "muche")) (cons (VAR "truc") (CST "a"))))
(test (STEP* (list NOP) trucmuche) trucmuche)
(test (STEP* (list NOP NOP) trucmuche) trucmuche)
(test (STEP* (list (SET (VAR "X") (CST "data")) (SET (VAR "X") (CST "data"))) 
             (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") (CST "data"))))
(test (STEP* (list (SET (VAR "X") (CST "data"))) 
             (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") (CST "data"))))
(test (STEP* () (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") NIL)))
(test (STEP* (list (WHILE (EQ (VAR "X") NIL) (list (SET (VAR "X") (CST "data")))) )
             (list (cons (VAR "X") NIL))) 
      (list (cons (VAR "X") (CST "data"))))
(test (STEP* (list (SET (VAR "truc") (CST 'a)) 
                   (SET (VAR "muche") (CST 'b))) 
             trucmuche) 
      (list (cons (VAR "muche") (CST 'b)) (cons (VAR "truc") (CST 'a))))
(test (STEP* (list (SET (VAR "A") (CONS (CST "truc") (CONS (CST "muche") NIL))) 
                   (SET (VAR "L") NIL) 
                   (WHILE (EQ (EQ (VAR "A") NIL)  NIL)
                          (list (SET (VAR "A") (TL (VAR "A"))) 
                                (SET (VAR "L") (CONS NIL (VAR "L")))))) 
             ()) 
      (list (cons (VAR "A") NIL) (cons (VAR "L") (CONS NIL (CONS NIL NIL)))))

;; assigns input values to a store
;; STORE-set : (list variable) -> (list expression) 
;;                               -> (list (pair variable expression))

(display "STORE-set")
(test (STORE-set (list (VAR "A") (VAR "B"))
                 (list (CST "truc") (CST "muche")))
      (list (cons (VAR "B") (CST "muche")) (cons (VAR "A") (CST "truc"))))

;; selects output values from a store
;; STORE-get : (list variables) -> (list (pair variable expression)) 
;;                               -> (list expression) 

(display "STORE-get")
(test (STORE-get (list (VAR "A") (VAR "B"))
                 (list (cons (VAR "A") (CST "truc")) (cons (VAR "B") (CST "muche"))))
      (list (CST "truc") (CST "muche")))

;; executes a program
;; 1- assigns input values to input variables in store
;; 2- computes STEP semantics of the program body; this returns a final context
;; 3- selects output values from final context
;; EXEC : program -> (list expression) -> (list expression) 

(define rreverse 
  (PROGR (list (VAR "X")) 
         (list (SET (VAR "Y") NIL) 
               (WHILE (VAR "X") 
                      (list (SET (VAR "Y") (CONS (HD (VAR "X")) (VAR "Y"))) 
                            (SET (VAR "X") (TL (VAR "X"))))))
         (list (VAR "Y"))))

(display "EXEC")
(test (EXEC (PROGR (list (VAR "X"))
                   (list (SET (VAR "X") (CONS (VAR "X") NIL)))
                   (list (VAR "X"))) (list NIL))
      (list (CONS NIL NIL)))
(test (EXEC rreverse (list (CONS (CST "truc") (CONS (CST "muche") NIL)))) 
      (list (CONS (CST "muche") (CONS (CST "truc") NIL))))

