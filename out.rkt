#lang racket
(require "csv.rkt" "scan.rkt")
(provide out-data out-mean out-rank n->srd)

;; data:
;; '((1 2..)
;;   (2 3..))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean data mean-file)
  (out-data mean-file (map list data)))

(define (out-rank day population rank-file)
  (out-data rank-file (append (list (list day))
                              (map list (rank population)))))

(define (n->srd s r d)
  (let ([pre-name (string-append
                   "R:/ibar/ibar-oct2/simuI"
                   "s" (number->string s) "r" (number->string r)
                   "d" (string-trim (number->string (* 10 d)) ".0"))])
    (list
     (string-append pre-name "mean.txt")
     (string-append pre-name "rank.txt")
     (string-append pre-name "plot.png"))))


#|
speed: 1 5 10 15 20
rounds: 1 5 10 20 50 100
delta: 0 .1 .2 ... 1



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
