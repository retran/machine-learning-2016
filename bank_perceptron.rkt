#lang racket

(require plot)

(define (get-points filename)
  (call-with-input-file filename
    (lambda (in)
      (read in))))

(define (select-points points criterion)
  (map (lambda (p) (list (first p) (second p)))
       (filter (lambda (p) (= criterion (third p))) points)))

(define (get-inputs point)
  (list (first point) (second point)))

(define (get-class point)
  (third point))

(define (reaction value)
  (if (< value 0) 0 1))

(define (associate weights inputs)
  (apply + (map * weights inputs)))

(define (neuron weights inputs)
  (reaction (associate weights inputs)))

(define (perceptron all-weights inputs)
  (map (lambda (weights) (neuron weights inputs)) all-weights))

(define (generate-weights n m)
  (map
   (lambda (i)
     (map (lambda (j)
            (random))
          (range m)))
   (range n)))

(define (educate-neuron weights inputs value)
  (let [(output (neuron weights inputs))]
    (map (lambda (w i)
           (+ w (* 0.1 i (- value output))))
         weights inputs)))

(define (educate-perceptron all-weights inputs values)
  (map
   (lambda
       (weights value) (educate-neuron weights inputs value))
   all-weights values))

(define (educate points)
  (let [(all-weights (generate-weights 2 2))]
    (for-each
     (lambda (p)
       (let [(inputs (get-inputs p))
             (class (get-class p))]
         (set! all-weights
               (educate-perceptron all-weights
                                   inputs
                                   (if (= class 0)
                                       '(0 1)
                                       '(1 0))))))
     points)
    all-weights))

(define (determine all-weights inputs)
  (let [(result (perceptron all-weights inputs))]
    (if (= (first result) 0) 0 1)))

(define all-points (get-points "bank_exp.txt"))
(define yes-points (select-points all-points 1))
(define no-points (select-points all-points 0))

(plot
 (list
  (points yes-points #:color "green")
  (points no-points #:color "red")))

(define all-weights (educate all-points))
(define p (first all-points))

(writeln all-weights)
(writeln p)

(define all-points-test (map
                         (lambda (p)
                           (list
                            (first p)
                            (second p)
                            (determine all-weights (get-inputs p))))
                         (get-points "bank.txt")))

(define yes-points-test (select-points all-points-test 1))
(define no-points-test (select-points all-points-test 0))

(plot
 (list
  (points yes-points-test #:color "green")
  (points no-points-test #:color "red")))

