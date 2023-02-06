(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cddr s))
)


(define (sign num)
  (cond ((< num 0)
         -1)
        ((= num 0)
         0)
        ((> num 0)
         1))
)


(define (square x) (* x x))

(define (pow x y)
  (cond ((= y 0)
         1)
        ((= y 1)
         x)
        ((even? y)
         (square (pow x (/ y 2))))
        (else
         (* x (square (pow x (- (/ y 2) 0.5))))))
)


(define (unique s)
  (if (null? s) nil
        (cons (car s) (unique (filter (lambda (x) (not (eq? x (car s)))) (cdr s)))))
)


(define (replicate x n)
  (define (replicate_helper lst t)
          (if (= t n)
              lst
              (replicate_helper (cons x lst) (+ t 1))))
  (replicate_helper nil 0)
)


(define (accumulate combiner start n term)
  (define (accumulate_helper ans t)
          (if (= t 0)
              ans
              (accumulate_helper (combiner ans (term t)) (- t 1))))
  (accumulate_helper start n)
)


(define (accumulate-tail combiner start n term)
    (define (accumulate_helper ans t)
          (if (= t 0)
              ans
              (accumulate_helper (combiner ans (term t)) (- t 1))))
  (accumulate_helper start n)
)


(define-macro (list-of map-expr for var in lst if filter-expr)
   `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)
