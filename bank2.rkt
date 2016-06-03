#lang racket

(require plot)

; 0.5 * k * x * x
(define (get-square k)
  (lambda (x) (* k x x)))

(define (get-points filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))

(define (select-points points criterion)
  (map (lambda (p) (list (first p) (second p)))
       (filter (lambda (p) (= criterion (third p))) points)))

(define (get-estimator points func-generator)
  (lambda (k)
    (let [(func (func-generator k))]
      (apply + (map
                (lambda (p)
                  (let* [(age (first p))
                         (salary (second p))
                         (ref-decision (third p))
                         (decision (cond
                                     [(< salary (func age)) 0]
                                     [else 1]))]
                    (* (sqr (- salary (func age)))
                     (abs (- ref-decision decision)))))
                points)))))

(define (get-diff func)
  (lambda (k)
    (/ (- (func (+ k 0.0001)) (func k)) 0.0001)))

(define (optimize func from to)
  (let* [(m (/ (+ from to) 2.0))
         (y (func m))]
    (cond [(< (abs y) 0.001) m]
          [(< y 0) (optimize func m to)]
          [else (optimize func from m)])))

(define all-points (get-points "bank_exp2.txt"))
(define yes-points (select-points all-points 1))
(define no-points (select-points all-points 0))

(define g (optimize (get-diff (get-estimator all-points get-square)) -10 10.0))

(writeln g)

(plot
 (list
  (points yes-points #:color "green")
  (points no-points #:color "red")
  (function (get-square g) 15 35)
  ))
