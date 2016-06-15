#lang racket

(require plot)

; 0.5 * k * x * x
(define (get-square k)
  (lambda (x) (* 0.5 k x x)))

(define (get-points filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))
  
(define (get-estimator points func-generator)
  (lambda (k)
    (let [(func (func-generator k))]
      (apply + (map
                (lambda (p)
                  (let [(x (first p))
                        (y (second p))]
                    (* (- y (func x)) (- y (func x)))))
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

(define spoints (get-points "newton.txt"))
(define g (optimize (get-diff (get-estimator spoints get-square)) 0 20.0))

(writeln g)
(plot
 (list (points spoints)
       (function (get-square g) 0 5)))
