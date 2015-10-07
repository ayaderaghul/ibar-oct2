(require "auto.rkt"
         "fit.rkt"
         "mass.rkt"
         "match.rkt"
         "mutation.rkt"
         "scan.rkt"
         "tv.rkt"
         "test.rkt"
         "out.rkt")


(define population-mean '())
(define pure-types '())


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

(define A (random-population 1 100))

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



(define (evolve population cycles speed mutation rounds-per-match delta)
  (let* ([l (length population)]
      ;;   [types-scanned (scan-4-types population)]
         [round-results (match-population population rounds-per-match delta)]
         [max-pay (apply max (flatten round-results))]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            l))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [before-mutation (shuffle (append survivors successors))]
         [new-population (mutate-populations
                          mutation
                          before-mutation)]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    (and (< average-payoff (* 3/4 max-pay))
         (out-rank cycles population delta))
   ;; (set! pure-types
   ;;       (append pure-types (list types-scanned)))
    (if (zero? cycles)
        (begin (plot-mean population-mean delta)
               (out-mean population-mean delta)
               population)
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match delta)
        )))
