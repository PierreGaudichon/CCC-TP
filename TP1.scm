;; TP 1
;; Cyril Leconte & Pierre Gaudichon


;; ================================================================
;;
;; Exercice 1
;; Calculer le rang de cantor d'un nombre.


;; Rang :: N x N -> N
;;         x , y -> Rang de cantor du couple (x . y)
(define Rang (lambda (x y)
  (+ (/ (* (+ x y) (+ x y 1)) 2) x)))


;; RangRec = Rang
(define RangRec (lambda (x y)
  (cond
    ((and (= x 0) (= x 0)) 0)
    ((= x 0) (+ (RangRec (- y 1) 0) 1))
    (else (+ (RangRec (- x 1) (+ y 1)) 1)))))


;; Enum :: N -> (N . N)
;;         n -> Le rang de cantor de n.
;;              Voir poly cours 2, page 32, EnumRat
(define Enum (lambda (n)
  (let* (
        [x+y (Enum-max n)]
        [x (Enum-x x+y n)])
    (cons x (- x+y x)))))


;; Enum-max :: N -> N
;;             n -> max {m | m(m+1)/2 <= n}
;;             Wrapper for recurcivity.
(define Enum-max (lambda (n)
  (Enum-rec n n)))
                   

;; Enum-rec :: N x N -> N
;;             m , n -> max {m | m(m+1)/2 <= n}
;;             Recursive, use Enum-m for the calcul of m(m+1)/2
(define Enum-rec (lambda (m n)
  (if (<= (Enum-m m) n)
      m
      (Enum-rec (- m 1) n))))
  

;; Enum-m :: N -> Q
;;           n -> m(m+1)/2
(define Enum-m (lambda (m)
  (/ (* m (+ m 1)) 2)))


;; Enum-x :: N   x N -> Q
;;           x+y , n -> x - (x+y)(x+y+1)/2
(define Enum-x (lambda (x+y n)
  (- n (Enum-m x+y))))


;; ================================================================
;;
;; Exercice 2


;; RangTriplet :: N x N x N -> N
;;                x , y , z -> Le rang de cantor de x y et z.
(define RangTriplet (lambda (x y z)
  (Rang (Rang x y) z)))


;; Enumtriplet :: N -> '(N N N)
;;                n -> Rang de cantor d'un triplet.
(define EnumTriplet (lambda (n)
  0))


;; ================================================================
;;
;; Exercice 2


;; RangList :: [N] (length = 2) -> N
;;              l               -> rang de cantor du n-uplet.
(define RangList (lambda (l)
  (if (= (length l) 2)
      (Rang (car l) (cadr l))
      (Rang (car l) (RangList (cdr l))))))


;; RangString :: String -> N
;;                  s   -> Rang de la chaine par cantor en base 256.
(define RangString (lambda (s)
  (RangList (map char->integer (string->list s)))))


(string->list "0")
(time (RangString "+++++++++++++++++"))