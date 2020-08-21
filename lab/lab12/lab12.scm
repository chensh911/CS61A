(define (partial-sums stream)
  (define (helper total s) (if (null? s) nil
  							(cons-stream (+ total (car s)) 
  										 (helper (+ total (car s)) (cdr-stream s)))))
  (helper 0 stream)
)