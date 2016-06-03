#lang racket

; 0.5 * k * x * x
(define (get-linear k)
  (lambda (x) (* x k)))

(define (get-random-age)
  (+ (random 50) 18.0))

(define (get-random-salary)
  (+ (random 200) 15.0))

(define (get-points n func)
  (map (lambda (n)
         (let* [(age (get-random-age))
                (salary (get-random-salary))
                (decision (cond
                            [(< salary (func age)) 0]
                            [else 1]))]
           (list age salary decision)))
       (range n)))

(call-with-output-file "bank_exp.txt"
    (lambda (out)
      (write (get-points 100 (get-linear 1.2)) out)))
