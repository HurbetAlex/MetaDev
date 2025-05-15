
#lang racket

(define-syntax-rule (swap! a b)
  (let ((temp a))
    (set! a b)
    (set! b temp)))

(define x 10)
(define y 20)

(printf "Before swap: x = ~a, y = ~a\n" x y)
(swap! x y)
(printf "After swap: x = ~a, y = ~a\n" x y)

(define-syntax-rule (repeat-until (body ...) condition)
  (let loop ()
    body ...
    (unless condition (loop))))

(let ([count 1])
  (repeat-until
    ((displayln (format "Count: ~a" count))
     (set! count (add1 count)))
    (> count 5)))

(define-syntax (with-resource stx)
  (syntax-case stx ()
    [(_ (acquire release) body ...)
     #'(let ([res acquire])
         (dynamic-wind
           (lambda () 'start)
           (lambda () body ...)
           (lambda () (release res))))]))

(define (fake-open) (displayln "Resource acquired") 'res)
(define (fake-close res) (displayln (format "Resource ~a released" res)))

(with-resource (fake-open fake-close)
  (displayln "Working with resource..."))
