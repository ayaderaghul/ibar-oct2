#lang racket
(require "auto.rkt")
(require "mass.rkt")
(provide
 set-immutable
 mutate
 mutate-populations)

(define (set-immutable automaton posn new-value)
  (append
   (take automaton posn)
   (list new-value)
   (drop automaton (add1 posn))))

(define (mutate an-auto)
  (let ([flatten-one (identify an-auto)]
        [r (random 10)]
        [c (random 3)])
    (apply automaton (set-immutable flatten-one r c))))

(define (mutate-populations countdown population)
  (let* ([l (length population)]
         [r (random l)]
         [mutated (mutate (list-ref population r))]
         [new-population (set-immutable population r mutated)])
    (if (zero? countdown)
        population
        (mutate-populations (sub1 countdown) new-population))))


(define (mutate-random population)
  (let ([n (random 19683)])
    (number->automaton
     (if (zero? (random 2)) n (+ 39366 n)))))

(define (mutate-random* population)
  (number->automaton (random 59049)))
