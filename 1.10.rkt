#lang racket

(require (for-syntax syntax/parse))

(define-syntax (define-state-machine stx)
  (syntax-parse stx
    [(_ name:id
        (state state-name:id
               ((evt:id => nxt:id) ...) ...) ...)
     #:with transition-rules
       (for/list ([s (in-list (syntax->list #'((state-name ((evt => nxt) ...)) ...)))])
         (define state (car s))
         (define transitions (cdr s))
         (define rules
           (for/list ([t (in-list (syntax->list (cadr transitions)))])
             (define ev (car t))
             (define ns (cadr t))
             #`[(eq? event '#,ev) '#,ns]))
         #`[(eq? current-state '#,state)
            (cond
              #,@rules
              [else current-state])])
     #'(define (name current-state event)
         (cond
           #,@transition-rules
           [else current-state]))]))

;; 🟢 Визначення автомата
(define-state-machine traffic-light
  (state green
    ((timeout => yellow)
     (power-out => off)))
  (state yellow
    ((timeout => red)
     (power-out => off)))
  (state red
    ((timeout => green)
     (power-out => off)))
  (state off
    ((power-on => red))))

;; ✅ Тести:
(displayln (traffic-light 'green 'timeout))      ; => 'yellow
(displayln (traffic-light 'yellow 'power-out))   ; => 'off
(displayln (traffic-light 'red 'timeout))        ; => 'green
(displayln (traffic-light 'off 'power-on))       ; => 'red
(displayln (traffic-light 'green 'unknown))      ; => 'green
