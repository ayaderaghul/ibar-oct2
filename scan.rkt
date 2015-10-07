#lang racket
(require "auto.rkt"
"match.rkt")
(provide scan-init
	scan
	scan-types
	scan-identify
	scan-types
	scan-4-types
        rank
        top
        contest)

;; SCAN

(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))
(define (scan-identify population)
  (foldl
   (lambda (au h)
     (hash-update h (identify au) add1 0))
   (hash)
   population))

(define (scan-init population)
  (foldl
   (lambda (au h)
     (hash-update h (automaton-init-claim au) add1 0))
   (hash)
   population))


(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))

(define (scan-types population)
  (let ([type-list (scan-init population)])
    (list
     (hash-ref* type-list 0)
     (hash-ref* type-list 1))))

(define (scan-4-types population)
(let ([ranking (scan-identify population)])
(list
(hash-ref* ranking (list 1 1 1 1 1 1 1 1 1 1))
(hash-ref* ranking (list 2 2 2 2 2 2 2 2 2 2)))))


(define (rank population)
  (let ([ranking (hash->list (scan-identify population))])
    (sort ranking > #:key cdr)))

(define (top t population)
  (let* ([flattened (map car (rank population))]
         [automaton (map (lambda (au)
                           (apply automaton au)) (take flattened t))])
    (for/list ([i t])
      (eval
       (list 'define (x->ax i)
             (list-ref automaton i))))))

(define (x->ax x)
  (string->symbol (string-append "a" (number->string x))))

(define (generate-ax a-list)
  (map x->ax a-list))

(define (contest an-auto a-list)
  (for/list ([n (length a-list)])
    (match-pair (list an-auto (list-ref a-list n)) 10)))
