
;; TESTING tools

(define show-all-tests #f)
(define any "ANY")

(define (test0 quoted-expression expression oracle)
  (let ((exp expression)
        (ora oracle)) 
    (cond
      ((and (or (equal? exp ora) 
                (equal? ora any))  
            (not show-all-tests))
       (begin (display ".")))
      (else
       ((lambda (test) (begin (newline) (display test) (display ": ")
                              (write quoted-expression)       
                              (newline) (display "eval to: ")
                              (write exp)        
                              (newline) (display "must be: ")
                              (write ora)           
                              (newline))) 
        (if (equal? exp ora) "TEST" "ERROR"))))))

(define-syntax test 
  (syntax-rules () ((test expr oracle) (test0 'expr expr oracle))))


