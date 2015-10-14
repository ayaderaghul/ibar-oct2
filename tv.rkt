#lang racket
(provide (all-defined-out))

(require plot "in.rkt")
(plot-new-window? #t)


;; TV

(define (plot-mean data plot-file)
  (let* ([l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors)
          #:width 800 #:height 400
          #:y-min 0 #:out-file plot-file)))

(define (plot-dynamic data N)
(plot (lines data)
	#:x-min 0 #:x-max N
        #:y-min 0 #:y-max N))


(define (plot-dynamics data-list N)
 [define l (length data-list)]
  [define data
    (for/list ([i (in-range l)])
      (lines (list-ref data-list i)))]
  (plot data
        #:x-min 0 #:x-max N
        #:y-min 0 #:y-max N))

(define (plot-dynamic-from-files file-list N)
  [define data-list (load-dynamics file-list)]
  (plot-dynamics data-list N))



#|

(define dynamic-frame (new frame%
                           [label "population average"]
                           [width 1000]
                           [height 400]))
(define dynamic-canvas (new canvas%
                            [parent dynamic-frame]))
(define dynamic-dc (send dynamic-canvas get-dc))
(define (plot-dynamic popu-length data)
  (plot/dc (lines (drop data 1)
                  #:x-min 0 #:x-max popu-length
                  #:y-min 0 #:y-max popu-length)
           dynamic-dc
           0 0 400 400))

(define (plot-mean data)
  (let* ([l (length data)]
         [coors (map list
                     (build-list l values)
                     data)])
    (plot/dc (lines coors
                    #:x-min 0 #:x-max l
                    #:y-min 0 #:y-max 11

                    )
             dynamic-dc
             0 0 1000 400)))

(define (plot-and-export file-name)
  (let* ([data (load-data file-name)]
         [l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors #:x-min 0 #:x-max l
                 #:y-min 0 #:y-max 11)
          #:width 1000 #:height 400
          #:x-label "cycles" #:y-label "average"
          #:out-file (string-replace file-name "txt" "png")
          )))

(define (plot-payoff-space pay-list)
  (plot/dc (points pay-list
                   #:x-min 0 #:x-max 820
                   #:y-min 0 #:y-max 820)
           dynamic-dc
           0 0
           400 400))
|#
