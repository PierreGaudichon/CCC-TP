
(load "test.ss")
(load "ccc.ss")

;; default root for new variables
;; newvar-root: string
(define newvar-root "Z")

;;(display "newvar-root")
;;(test newvar-root newvar-root)

;; initial value for numbers of new variables
;; newvar-num: int
(define newvar-num 0)

;;(display "newvar-num")
;;(test newvar-num newvar-num)

;; gets a newvar number and increments the newvar counter
;; newvar-get: none
(define (newvar-get) (let ((nv newvar-num))
                       (begin (set! newvar-num (+ nv 1))
                              nv)))

;;(display "newvar-get")
;;(test (newvar-get) 0)
;;(test (newvar-get) 1)

;; resets the newvar counter to 0
;; newvar-reset: none
(define (newvar-reset) (set! newvar-num 0))

;;(display "newvar-reset")
;;(test newvar-num 2)
;;(newvar-reset)
;;(test newvar-num 0)

;; builds a new variable
;; newvar-make: string -> as
(define (newvar-make root)
  (let ((num (newvar-get)))
    (VAR (string-append root (number->string num)))))

;;(display "newvar-make")
;;(test (newvar-make "A") (VAR (string-append "A" "0")))

