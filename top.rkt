#lang racket
(require "auto.rkt")
(provide rank 
	top 
	top-identify)

(define (rank a-hash)
  (sort (hash->list a-hash) #:key cdr >))

(define (n->xn n)
  (string->symbol
   (string-append "x" (number->string n))))

(define (top t a-hash)
  (let* ([top-list (map car (take (rank a-hash) t))]
         [l (length top-list)])
    (for/list ([i l])
      (eval
       (list 'define (n->xn i)
             (list-ref top-list i))))))

(define (top-identify t a-hash)
  (let* ([top-list (map car (take (rank a-hash) t))]
         [l (length top-list)])
    (for/list ([i l])
      (eval
       (list 'define (n->xn i)
             (apply make-automaton (list-ref top-list i)))))))


(define (rank-payoff criterion population rounds-per-match)
  (let ([payoff-list (flatten (match-population population rounds-per-match))])
    (sort (hash->list (scan payoff-list)) #:key criterion >)))


