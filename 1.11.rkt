#lang racket

(require racket/match)

(define (split-path path)
  (map (λ (s)
         (if (and (positive? (string-length s))
                  (char=? (string-ref s 0) #\:))
             (string->symbol (substring s 1))
             s))
       (string-split path "/")))

(define (match-path route-path req-path)
  (define route (split-path route-path))
  (define req   (split-path req-path))
  (when (= (length route) (length req))
    (let loop ([rs route] [qs req] [acc '()])
      (cond
        [(empty? rs) (reverse acc)]
        [(symbol? (first rs))
         (loop (rest rs) (rest qs) (cons (first qs) acc))]
        [(string=? (first rs) (first qs))
         (loop (rest rs) (rest qs) acc)]
        [else #f]))))

(define (make-router clauses)
  (λ (method path . args)
    (define result
      (for/fold ([res #f])                             
                ([clause (in-list clauses)])           
        (if res 
            res                                      
            (match clause
              [(list m route handler)
               (and (eq? method m)
                    (match (match-path route path)
                      [(list ps ...) (apply handler (append ps args))]
                      [else #f]))]
              [(list m route 'params parms handler)
               (and (eq? method m)
                    (match (match-path route path)
                      [(list ps ...) (apply handler (append ps args))]
                      [else #f]))]
              [else #f]))))
    (or result 404)))

(define (user-handler id)
  (format "Профіль користувача з ID: ~a" id))
(define (page-handler name)
  (format "Сторінка: ~a" name))

(define app
  (make-router
   (list
    (list 'GET "/" (λ () "Привіт, світе!"))
    (list 'POST "/login" 'params '(user pass)
          (λ (user pass) (format "Вхід: ~a/~a" user pass)))
    (list 'POST "/login/:id" 'params '(user pass)
          (λ (id user pass) (format "Вхід: ~a/~a/~a" id user pass)))
    (list 'GET "/user/:id" user-handler)
    (list 'GET "/page/:name" page-handler)
    )))


(module+ test
  (require rackunit)
  (check-equal? (app 'GET "/")                 "Привіт, світе!")
  (check-equal? (app 'POST "/login" "іван" "секрет")
                "Вхід: іван/секрет")
  (check-equal? (app 'POST "/login/8" "іван" "секрет")
                "Вхід: 8/іван/секрет")
  (check-equal? (app 'GET "/user/42")
                "Профіль користувача з ID: 42")
  (check-equal? (app 'GET "/page/about")
                "Сторінка: about")
  (check-equal? (app 'DELETE "/oops")          404)
  (displayln "Усі тести пройдені успішно!"))
