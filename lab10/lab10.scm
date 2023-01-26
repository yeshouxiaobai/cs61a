(define (over-or-under num1 num2)
  (if (< num1 num2)
    -1
    (if(= num1 num2)
      0
      1
    )
  )
)

;;; Tests
(over-or-under 1 2)
; expect -1
(over-or-under 2 1)
; expect 1
(over-or-under 1 1)
; expect 0


(define (filter-lst fn lst)
 (if (null? lst)
     nil
     (if (fn (car lst))
         (cons (car lst) (filter-lst fn (cdr lst)))
         (filter-lst fn (cdr lst))))  
)

;;; Tests
(define (even? x)
  (= (modulo x 2) 0))
(filter-lst even? '(0 1 1 2 3 5 8))
; expect (0 2 8)


(define (make-adder num)
  (lambda (inc) (+ num inc))
)

;;; Tests
(define adder (make-adder 5))
(adder 8)
; expect 13


(define lst
  (list (list 1) 2 (list 3 4) 5)
)


(define (composed f g)
  (lambda (inc) (f (g inc)))
)


(define (remove item lst)
  (if (null? lst)
      nil
      (if (= (car lst) item)
          (remove item (cdr lst))
          (cons (car lst) (remove item (cdr lst)))))
)


;;; Tests
(remove 3 nil)
; expect ()
(remove 3 '(1 3 5))
; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))
; expect (3 1 4 4)


(define (no-repeats s)
  (define (judge item lst)
    (if (null? lst)
        #t
        (if (= (car lst) item)
            #f
            (judge item (cdr lst)))))
  (define (recurse lst remain)
    (if (null? remain)
        lst
        (if (judge (car remain) lst)
            (recurse (cons (car remain) lst) (cdr remain))
            (recurse lst (cdr remain)))))
  (define (reverse result lst)
    (if (null? lst)
        result
        (reverse (cons (car lst) result) (cdr lst))))
  (define b (recurse () s))
  (reverse () b)
)


(define (substitute s old new)
  'YOUR-CODE-HERE
)


(define (sub-all s olds news)
  'YOUR-CODE-HERE
)

