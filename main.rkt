#lang racket
(provide (all-defined-out))

(require "auto.rkt"
         "fit.rkt"
         "mass.rkt"
         "match.rkt"
         "mutation.rkt"
         "scan.rkt"
         "tv.rkt"
         "test.rkt"
         "out.rkt")

;; create population
(define (random-population* n-automata-per-type types)
  (shuffle
   (flatten
    (for/list ([i types])
      (make-list n-automata-per-type i)))))

(define (random-population
         n-automata-per-type n-types)
  (random-population*
   n-automata-per-type
   (for/list ([i n-types])
     (number->automaton (random 59049)))))

;(define A (random-population 1 100))

(define (random-one-shot-population
         h-n-types m-n-types l-n-types)
  (shuffle
   (append
    (random-population*
     1 (for/list ([l l-n-types])
         (number->automaton (random 19683))))
    (random-population*
     1 (for/list ([m m-n-types])
         (number->automaton (+ 19683 (random 19683)))))
    (random-population*
     1 (for/list ([h h-n-types])
         (number->automaton (+ 39366 (random 19683))))))))

(define (evolve population cycles speed mutation rounds-per-match delta file-list)
  (define N (length population))
  (define-values (result popu 2-types)
    (for/fold ([result '()]
               [population population]
               [2-types '()])
              ([i cycles])
      [define types (scan-types population)]
      [define round-results (match-population population rounds-per-match delta)]
      [define total (apply + (flatten round-results))]
      [define max-payoff (apply max (flatten round-results))]
      [define average-payoff (exact->inexact
                              (/ total N))]
      [define accum-fitness (accumulated-payoff-percentages (flatten round-results))]
      [define survivors (drop population speed)]
      [define successors
        (randomise-over-fitness accum-fitness population speed)]
      [define before-mutation (shuffle (append survivors successors))]
      [define new-population (mutate-populations
                              mutation
                              before-mutation)]
      (out-rank i new-population 6 (second file-list))
      (values (cons average-payoff result)
              new-population
              (cons types 2-types))))
  (out-mean (reverse result) (first file-list))
  (plot-mean (reverse result) (third file-list))
 ; (plot-dynamic (reverse 2-types) N)
 ; (out-mean (reverse 2-types) (first file-list))
  )

;; run mass

(define (run-one s r d)
  [define B (random-population 1 100)]
  [define name-list (n->srd s r d)]
  (time (evolve B 500000 s 1 r d name-list)))

(define (run-many-oneshot points d)
  (for ([i (length points)])
    [define p (list-ref points i)]
    [define B (random-one-shot-population (last p) (second p) (first p))]
    (time (evolve B 1000 10 0 1 d
                  (list (string-append
                         "R:/ibar/1s0d"
                         (string-trim (number->string (* 10 d)) ".0")
                         "p"
                         (number->string i)
                         ".txt"))))))
                       

(define point-list
  (list
  (list 900 50 50)
  (list 800 50 150)
  (list 50 50 900)
  (list 50 150 800)
  (list 800 150 50)
  (list 700 250 50)))

(define (run-many list-of-speeds list-of-rounds list-of-deltas)
  (for* ([i (in-list list-of-speeds)]
         [j (in-list list-of-rounds)]
	[k (in-list list-of-deltas)])
    [define B (random-population 1 100)]
    [define name-list (n->srd i j k)]
    (time (evolve B 500000 i 1 j k name-list))))

(define speed-list
  (list 1 5 10 20))
(define round-list
  (list 1 10 20 50))
(define delta-list
  (list 0 .2 .8 .9 1))
