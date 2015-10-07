#lang racket
(require "auto.rkt")
(provide match-claims
	match-pair
	match-population)

;; previous-claim is a list of two claims
;; - the agent's own claim
;; - the opponent's claim
(define (next-claim automaton previous-claims)
  (let ([look-up
         (cond
          [(equal? previous-claims '(2 2)) automaton-hh]
          [(equal? previous-claims '(2 1)) automaton-hm]
          [(equal? previous-claims '(2 0)) automaton-hl]
          [(equal? previous-claims '(1 2)) automaton-mh]
          [(equal? previous-claims '(1 1)) automaton-mm]
          [(equal? previous-claims '(1 0)) automaton-ml]
          [(equal? previous-claims '(0 2)) automaton-lh]
          [(equal? previous-claims '(0 1)) automaton-lm]
          [(equal? previous-claims '(0 0)) automaton-ll])])
    (look-up automaton)))
;; each outcome (0,1,2) is equivalent to payoff (2,5,8)
(define (payoff outcome)
  (cond [(zero? outcome) 2]
        [(= outcome 1) 5]
        [(= outcome 2) 8]
        ))

;; input: claims (l,m,h) ~ (0,1,2)
;; output: payoff (0,2,5,8)
(define (match-claims claims)
  (if (<= (apply + claims) 2)
      (map payoff claims)
      (list 0 0)))

(define (match-pair* au1 au2 results previous-claims countdown)
  (if (zero? countdown)
      results
      (match-pair* au1 au2
                   (append results (list
                                    (match-claims previous-claims)))
                   (list (next-claim au1 previous-claims)
                         (next-claim au2 (reverse previous-claims)))
                   (sub1 countdown))))

;; match a pair of automaton for n rounds
;; return a list of round results
(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (first automaton-pair)
               (second automaton-pair)
               '()
               (map automaton-init-claim automaton-pair)
               rounds-per-match))

;; delta
;(define delta 1)
(define (take-delta* f round-results delta)
  (let ([first-auto (map f round-results)])
    (for/list ([i (length first-auto)])
      (* (expt delta i) (list-ref first-auto i)))))

(define (take-delta round-results delta)
  (map (lambda (x) (apply + (take-delta* x round-results delta)))
       (list first second)))

;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))


(define (match-population population rounds-per-match delta)
  (for/list ([i (/ (length population)
                   2)])
    (take-delta
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match)
     delta)))
