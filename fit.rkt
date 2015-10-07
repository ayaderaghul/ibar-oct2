#lang racket
(require
  "match.rkt")

(provide accumulate
         payoff-percentages
         accumulated-fitness
         randomise-over-fitness
	randomisation-test)

;; from the matching result, calculate the fitness
(define (reductions-h f accumulated init a-list)
  (if (null? a-list)
      accumulated
      (let ((next-init (f init (first a-list))))
        (reductions-h f
                      (append accumulated (list next-init))
                      next-init
                      (rest a-list)))))
(define (reductions f init a-list)
  (if (null? a-list)
      '()
      (reductions-h f '() init a-list)))

(define (reductions* f a-list)
  (let ([init (first a-list)])
    (reductions-h f (list init) init (rest a-list))))

(define (accumulate a-list)
  (reductions* + (cons 0 a-list)))

(define (payoff-percentages payoff-list)
  (let ([s (apply + payoff-list)])
    (if (zero? s)
        (for/list ([i (length payoff-list)])
          (/ 1 (length payoff-list)))
        (for/list ([i (length payoff-list)])
          (/ (list-ref payoff-list i)
             s)))))

(define (accumulated-fitness population rounds-per-match)
  (accumulate
   (payoff-percentages
    (flatten
     (match-population population rounds-per-match)))))

;; generate new automaton by randomising over the fitness vector
(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (let
      ([len (length population)])
    (for/list
        ([n speed])
      (let ([r (random)])
        (for/and ([i len])
          #:break (< r (list-ref accumulated-payoff-percentage i))
          (list-ref population i))))))


(define (randomisation-test an-accumulated-list N)
  (for/list
      ([n 20])
    (let ([r (random)])
      (for/and ([i N])
        #:break (< r (list-ref an-accumulated-list i))
        i))))
