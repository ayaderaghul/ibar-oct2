#lang racket

(require "auto.rkt")
(provide create-test-population)

(define (create-test-population high medium low accom)
   (shuffle
   (append
    (make-list high all-highs)
    (make-list medium all-mediums)
    (make-list low all-lows)
    (make-list accom accommodator))))

;; test 1s is in the main.rkt
