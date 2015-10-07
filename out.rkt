#lang racket
(require "csv.rkt" "scan.rkt")
(provide out-data out-mean out-rank)

;; data:
;; '((1 2..)
;;   (2 3..))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))


(define (generate-mean-txt delta)
  (string-append "R:/ibar/simuII/simu"
                 (string-trim (number->string (* 10 delta)) ".0")
                 "/mean.txt"))


(define (generate-rank-txt delta)
  (string-append "R:/ibar/simuII/simu"
                 (string-trim (number->string (* 10 delta)) ".0")
                 "/rank.txt"))


(define (out-mean data delta)
  (out-data (generate-mean-txt delta) (map list data)))

(define (out-rank day population delta)
  (out-data (generate-rank-txt delta) (append (list (list day))
                           (map list (rank population)))))

#|

;; TRIAL RUN
; (load "ibar.rkt")
; (define A (random-population 1 150))
; (define A1 (evolve A 20000 8 1 1 50))
; (send dynamic-frame show #t)
; (plot-mean population-mean)
; (rank (scan-identify A1))

(define (run-simulation machine-per-type machines cycles speed mutant-per-type mutants rounds-per-match file-list)
  (define A (random-population machine-per-type machines))
  (define A1 (evolve A cycles speed mutant-per-type mutants rounds-per-match
                     (first file-list) (last file-list)
                     ))
  (print "hi"))

machine-per-type 1
machines 100
cycles 100000
speed 5/10/15/20
mutant-per-type 1
mutants 1
rounds-per-match 1/5/10/15/20
file-mean
file-rank


(define (n->sr s r)
  (let ([pre-name (string-append
                   "R:/report15/"
                   "s" (number->string s) "r" (number->string r))])
    (list
     (string-append pre-name "m.txt")
     (string-append pre-name "r.txt"))))

(define (n->mrt n)
  (let ([pre-name (string-append
                   "R:/"
                   (number->string n))])
    (list
     (string-append pre-name "m.txt")
     (string-append pre-name "r.txt")
     (string-append pre-name "t.txt"))))

(define speed-list (list 5 10 15 20))
(define speed-list-2 (list 10 15 20))

(define rounds-list (list 1 5 10 15 20))
(define rounds-list-1 (list 1 5 10))
(define rounds-list-2 (list 15 20))

(define numbers-list
  (list (list 400 500 100)
        (list 600 300 100)
        (list 800 100 100)
        (list 600 100 300)
        (list 400 100 500)
        (list 200 100 700)
        (list 0 100 900)
        (list 100 200 700)
        (list 200 300 500)
        ))

(define (run-one-shot number-list file-type)
  (define A (random-one-shot-population (first number-list)
                                        (second number-list)
                                        (last number-list)))
  (define A1 (evolve A 500 100 1 5 1 "a" "b"
                     file-type))
  (print "hi"))

(define (run-one-shots number-list file-type)
  (for ([i (length number-list)])
    (run-one-shot (list-ref number-list i)
                  file-type)
;    (set! population-mean (list 0))
    (set! pure-types (list (list 0 0)))))

(define (run-all list-of-speeds list-of-rounds)
  (for* ([i (in-list list-of-speeds)]
         [j (in-list list-of-rounds)])
    (run-simulation 1 100 100000 i 1 1 j (n->sr i j))
    (set! population-mean (list 0))))

(define (export-all-pics list-of-speeds list-of-rounds)
  (for* ([s (in-list list-of-speeds)]
         [r (in-list list-of-rounds)])
    (plot-and-export (first (n->sr s r)))))

(define (load-data file-name)
  (let ([data (read-csv-file file-name)])
    (map string->number (flatten data))))

(define (load-and-plot-mean file-mean)
  (let ([data (read-csv-file file-mean)])
    (plot-mean (map string->number (flatten data)))
    ))

(define (load-coors file-type)
  (let ([data (read-csv-file file-type)])
    (map (lambda (x) (map string->number x)) data)))

(define (load-and-divide number-list file-type)
  (let* ([l (length number-list)]
         [data (load-coors file-type)]
         [data-l (length data)]
         [unit-l (/ data-l l)])
    (for/list ([i l])
      (drop (take (drop data (* i unit-l)) unit-l) 1))))


(define (load-and-plot-rd popu-length number-list file-type)
  (let* ([data-list (load-and-divide number-list file-type)]
         [l (length data-list)])
    (plot
     (for/list ([i l])
       (lines (list-ref data-list i))
       )
     #:x-min 0 #:x-max popu-length
     #:y-min 0 #:y-max popu-length
     #:width 400 #:height 400
     #:x-label "low" #:y-label "medium"
     #:out-file (string-replace file-type "txt" "png"))))

(define (load-and-plot-type popu-length file-type number-list)
  (let ([l (length number-list)]
        [data (load-coors file-type)])
    (plot-dynamic popu-length data)))


(define (plot-and-export-type popu-length file-name)
  (let ([RD-coors (drop (load-coors file-name) 1)])
    (plot (lines RD-coors #:x-min 0 #:x-max popu-length
                 #:y-min 0 #:y-max popu-length)
          #:width 400 #:height 400
          #:x-label "low" #:y-label "medium"
          #:out-file (string-replace file-name "txt" "png")
          )))

(define (export-all-type-pics popu-length list-of-numbers)
  (let ([l (length list-of-numbers)])
    (for ([i l])
      (plot-and-export-type popu-length (last (n->mrt i))))))


(define (load-and-cut file-name)
  (let ([data (load-data file-name)])
    (out-data (string-replace file-name "report" "report2")
              (map list (take-right data 100000)))))


(define (colorise n)
  (cond [(<= n 4.2) 0]
        [(and (> n 4.2)
              (<= n 4.4)) 6]
        [(and (> n 4.4)
              (<= n 4.6)) 1]
        [(and (> n 4.6)
              (<= n 4.8)) 3]
        [(> n 4.8) 2]))

(define (load-mean folder-num s r)
  (mean (drop (load-data
               (string-append "R:/report"
                              (number->string folder-num)
                              "/s" (number->string s)
                              "r" (number->string r)
                              "m.txt"))
              1)))

(define (load-means folder-num)
  (for*/list ([i (in-list speed-list)]
              [j (in-list rounds-list)])
    (load-mean folder-num i j)))

(define point-coors
  (for*/list
      ([i (in-list speed-list)]
       [j (in-list rounds-list)])
    (list i j)))



(define (find-coors-h color a-list)
  (filter true?
          (for/list ([i (length a-list)])
            (and (= color (list-ref a-list i)) i))))

(define (find-coors color a-list)
  (let ([flat-coors (find-coors-h color a-list)])
    (for/list ([i (in-list flat-coors)])
      (list (list-ref speed-list (quotient i 5))
            (list-ref rounds-list (remainder i 5))))))

(define (true? x)
  (not (false? x)))


(define (plot-color color folder-num)
  (let* ([mean-list (load-means folder-num)]
         [color-list (map colorise mean-list)])
    (plot
     (points
      (find-coors color color-list)
      #:color color #:size 20))))

(define (plot-colors folder-num)
  (let* ([mean-list (load-means folder-num)]
         [color-list (map colorise mean-list)]
         [colors (remove-duplicates color-list)])
    (plot
     (for/list
         ([i (length colors)])
       (points (find-coors (list-ref colors i) color-list)
               #:color (list-ref colors i) #:size 20
               )))))


(define (chop automaton)
  (let ([body (drop automaton 1)])
    (list
     (take body 3)
     (take (drop body 3) 3)
     (take-right body 3))))

(define (scan-duplicate body-part)
  (let ([a (first body-part)]
        [b (second body-part)]
        [c (third body-part)])
    (cond
     [(= a b c) (list "H,M,L" "H,M,L" "H,M,L")]
     [(= a b) (list "H,M" "H,M" "L")]
     [(= a c) (list "H,L" "M" "H,L")]
     [(= b c) (list "H" "M,L" "M,L")]
     [else (list "H" "M" "L")]
     )))

(define (scan-duplicates automaton)
  (flatten
   (for/list ([i 3])
     (scan-duplicate (list-ref (chop automaton) i)))))

(define trajectories
  (list
   "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]"
   "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]"
   "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]"))

(define (mix-inputs automaton)
  (map list
     (drop automaton 1)
     (scan-duplicates automaton)))

(define (make-body automaton)
  (let ([inputs (mix-inputs automaton)])
    (list*
     "Graph[{-1 -> ~a"
     (remove-duplicates
      (for/list ([i 9])
        (apply format
               (list-ref trajectories i)
               (list-ref inputs i))))

     )))


(define (generate-code a-list posn x)
  (let ([automaton (list-ref a-list posn)])
    (format
     (string-append*
      (append
       (list "~aGraph =
")
       (cdr
        (append*
         (map (lambda (x) (list ", " x))
              (make-body automaton))))
       (list "},
   EdgeShapeFunction -> GraphElementData[\"EdgeShapeFunction\", \"FilledArrow\"],
   VertexStyle -> LightGray,
   VertexShapeFunction -> VertexCircle,
   VertexLabels -> {0 -> Placed[\"L\", Center], 1 -> Placed[\"M\", Center], 2 -> Placed[\"H\", Center]}
  ];
")
       (list
        "Export[\"~a.png\", ~aGraph];
"
        )))
     (name x posn)
     (first automaton)
     (name x posn)
     (name x posn))))

; - 1 HH HM HL MH MM ML LH LM LL
;   1  1  1  2  0  1  2  2  1  1
(define (generate-codes a-list x)
  (for/list ([i (length a-list)])
    (generate-code a-list i x)))

(define (export-codes a-list x)
  (for ([i (length a-list)])
    (with-output-to-file "auto-code.txt"
      (lambda () (printf (generate-code a-list i x)))
      #:exists 'append)))

(define (name x n)
  (string->symbol (string-append x (number->string n))))

(define list-1
  (list
   (list 2 2 2 2 0 2 0 0 0 2)
   (list 1 0 0 2 2 0 1 0 2 2)
   (list 2 2 2 2 1 0 0 0 0 0)
   (list 2 2 2 2 1 2 0 0 0 0)))

(define list-2
  (list
   (list 2 2 2 2 0 0 0 0 2 2)
   (list 2 0 2 2 1 1 0 0 2 2)
   (list 2 2 2 2 0 0 0 0 2 0)
   (list 1 2 2 2 0 0 0 0 2 0)
   (list 2 2 2 2 0 2 0 0 2 0)
   ))

(define list-3
  (list
   (list 0 2 2 2 0 2 0 1 1 0)
   (list 1 0 1 2 1 2 0 2 1 2)
   (list 1 2 2 2 1 2 2 0 1 2)
   (list 0 2 2 2 0 2 0 1 2 0)
   (list 0 2 2 0 0 2 0 1 1 0)
   (list 1 2 2 2 1 2 2 2 1 2)
   (list 1 1 2 2 0 0 0 0 1 2)
   (list 1 2 1 2 1 2 0 2 1 2)
   (list 0 2 2 0 0 2 0 1 1 1)
   (list 1 2 2 2 1 2 2 1 1 2)
   (list 1 2 2 2 1 2 0 2 1 2)
   ))

(define (resurrect x)
  (map eval
       (for/list ([i (length list-1)])
         `(define ,(name x i) (apply make-automaton (list-ref list-1 ,i))))))

(define (map-string a-list)
  (map number->string a-list))

(define (map-& a-nested-list)
  (string-append
   (string-append*
    (flatten
     (map (lambda (y) (list " " y))
          (flatten
           (map (lambda (x) (list "&" x))
                (map map-string a-nested-list))))))
   "\\\\"))

(define (export-latex result)
  (out-data "result" (map list (map map-& result))))

|#
