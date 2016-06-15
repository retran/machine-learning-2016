#lang racket

; 0.5 * k * x * x
(define (get-square k)
  (lambda (x) (* 0.5 k x x)))

(define (get-error)
  (/ (- (random 10) 5) 10))

(define (get-points func points)
  (map
   (lambda (x)
     (list x (+ (func x) (get-error))))
   points))

(define (random-points n)
  (map (lambda (n) (/ (random 5000) 1000.0)) (range n)))

(call-with-output-file "newton.txt"
    (lambda (out)
      (write (get-points (get-square 9.8) (random-points 30)) out)))
