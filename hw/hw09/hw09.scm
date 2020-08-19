
; Tail recursion

(define (replicate x n)
  (define (helper lst i)
      (if (= i n)
        lst
        (helper (cons x lst) (+ i 1))))
  (helper nil 0)
  )

(define (accumulate combiner start n term)
  (if (= 1 n)
      (term start)
      (combiner (term start)
                (accumulate combiner (+ 1 start) (- n 1) term)))
)

(define (accumulate-tail combiner start n term)
  (define (helper total now-num i) 
      (if (= i n) total
                  (helper (combiner total (term now-num)) (+ now-num 1) (+ 1 i))))
  (helper 1 start 0)
)

; Streams

(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (map-stream (lambda (x) (+ x 3)) (cons-stream 0 multiples-of-three))
)


(define (nondecreastream s)
    (define (helper final sofar last-element s)
        (cond
            ((null? s) final)
            ((> last-element (car s)) (cons-stream (nondecreastream1 (cdr-stream s)) final))
            (else (helper final (cons (car s) sofar) (car s) (cdr-stream s)))
        )
    )
    (helper '() '() 0 s)
)

(define (reverse-stream l)
  (if (null? l)
    nil
    (append (reverse-stream (cdr-stream l)) (cons-stream (car l)))
  )
)


(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))