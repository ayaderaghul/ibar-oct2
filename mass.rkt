#lang racket
(require "auto.rkt")
(provide base10->base3
	number->automaton
	automaton->number)

;; mass production
(define (base10->base3 n)
  (~r n #:base 3 #:min-width 10 #:pad-string "0"))

(define (char->digit c)
  (case c
    ;; (map (lambda (i) (format "[(#\\~a) ~a]" i i))
    ;;      (range 0 10))
    [(#\0) 0]
    [(#\1) 1]
    [(#\2) 2]
    [(#\3) 3]
    [(#\4) 4]
    [(#\5) 5]
    [(#\6) 6]
    [(#\7) 7]
    [(#\8) 8]
    [(#\9) 9]))

(define (base3->digits a-string)
  (map char->digit (string->list a-string)))

(define (number->automaton n)
  (apply automaton (base3->digits (base10->base3 n))))


(define (automaton->number automaton)
  (string->number
   (apply string-append (map number->string automaton))
   3))
