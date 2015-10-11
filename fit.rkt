#lang racket
(require "match.rkt")

(provide (all-defined-out))

;; FITNESS
;; from matching result, calculate the fitness

(define (accumulated-payoff-percentages payoff-list)
  (define payoff-sum (apply + payoff-list))
  (define-values (accumulated _)
    (for/fold ([accumulated (list 0)]
               [init 0])
              ([y (in-list payoff-list)])
      [define next-init (+ init (/ y payoff-sum))]
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (for/list ([ n speed])
    [define r (random)]
    (for/and ([p (in-list population)]
              [a (in-list accumulated-payoff-percentage)]
              #:break (< r a))
      p)))

(define (randomisation-test an-accumulated-list N)
  (for/list
      ([n 20])
    (let ([r (random)])
      (for/and ([i N])
        #:break (< r (list-ref an-accumulated-list i))
        i))))
