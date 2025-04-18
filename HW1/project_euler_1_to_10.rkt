#lang racket

;; Project Euler Problem 1
;; Find the sum of all the multiples of 3 or 5 below 1000.
(define (problem-1)
  (define (helper i acc)
    (if (>= i 1000)
        acc
        (if (or (= (remainder i 3) 0) (= (remainder i 5) 0))
            (helper (+ i 1) (+ acc i))
            (helper (+ i 1) acc))))
  (helper 1 0))


;; Problem 2
;; Sum even Fibonacci numbers not exceeding 4 million
(define (problem-2)
  (define (fib a b acc)
    (if (> a 4000000)
        acc
        (fib b (+ a b) (if (= (remainder a 2) 0) (+ acc a) acc))))
  (fib 1 2 0))


;; Problem 3
;; Largest prime factor of 600851475143
(define (problem-3)
  (define (is-divisible? n d)
    (= (remainder n d) 0))
  (define (smallest-divisor n d)
    (if (> (* d d) n)
        n
        (if (is-divisible? n d)
            d
            (smallest-divisor n (+ d 1)))))
  (define (largest-prime-factor n)
    (define (factor n)
      (let ((d (smallest-divisor n 2)))
        (if (= d n)
            d
            (factor (/ n d)))))
    (factor n))
  (largest-prime-factor 600851475143))


;; Problem 4
;; Largest palindrome product of two 3-digit numbers
(define (problem-4)
  (define (reverse-number n acc)
    (if (= n 0) acc (reverse-number (/ n 10) (+ (* acc 10) (remainder n 10)))))
  (define (is-palindrome? n)
    (= n (reverse-number n 0)))
  (define (loop a b max)
    (if (< a 100)
        max
        (if (< b 100)
            (loop (- a 1) 999 max)
            (let* ((p (* a b))
                   (new-max (if (and (is-palindrome? p) (> p max)) p max)))
              (loop a (- b 1) new-max)))))
  (loop 999 999 0))


;; Problem 5
;; Smallest multiple divisible by all numbers from 1 to 20
(define (problem-5)
  (define (gcd a b)
    (if (= b 0) a (gcd b (remainder a b))))
  (define (lcm a b)
    (/ (* a b) (gcd a b)))
  (define (loop n acc)
    (if (> n 20)
        acc
        (loop (+ n 1) (lcm acc n))))
  (loop 2 1))


;; Problem 6
;; Difference between square of sums and sum of squares
(define (problem-6)
  (define (loop i sum sq)
    (if (> i 100)
        (- (* sum sum) sq)
        (loop (+ i 1) (+ sum i) (+ sq (* i i)))))
  (loop 1 0 0))


;; Problem 7
;; 10001st prime number
(define (problem-7)
  (define (is-prime? n)
    (define (loop d)
      (cond ((= n 2) #t)
            ((= (remainder n d) 0) #f)
            ((> (* d d) n) #t)
            (else (loop (+ d 1)))))
    (loop 2))
  (define (find-prime i count)
    (if (= count 10001)
        (- i 1)
        (if (is-prime? i)
            (find-prime (+ i 1) (+ count 1))
            (find-prime (+ i 1) count))))
  (find-prime 2 0))


;; Problem 8
;; Largest product of thirteen adjacent digits
(define (problem-8)
  (define digits
    '(7 3 1 6 7 1 7 6 5 3 1 3 3 0 6 2 4 9 1 9 2 2 5 1 1 9 6 7 4 4 2 6 5 7 4 7 4 2 3 5 5 3 4 9 1 9 4 9 3 4 9 6 9 8 3 5 2 0 3 1 2 7 7 4 5 0 6 3 2 6 2 3 9 5 7 8 3 1 8 0 1 6 9 8 4 8 0 1 8 6 9 4 7 8 8 5 1 8 4 3 8 5 8 6 1 5 6 0 7 8 9 1 1 2 9 4 9 4 9 5 4 5 9 5 0 1 7 3 7 9 5 8 3 3 1 9 5 2 8 5 3 2 0 8 8 0 5 5 1 1 1 2 5 4 0 6 9 8 7 4 7 1 5 8 5 2 3 8 6 3 0 5 0 7 1 5 6 9 3 2 9 0 9 6 3 2 9 5 2 2 7 4 4 3 0 4 3 5 5 7 6 6 8 9 6 6 4 8 9 5 0 4 4 5 2 4 4 5 2 3 1 6 1 7 3 1 8 5 6 4 0 3 0 9 8 7 1 1 1 2 1 7 2 2 3 8 3 1 1 3 6 2 2 2 9 8 9 3 4 2 3 3 8 0 3 0 8 1 3 5 3 3 6 2 7 6 6 1 4 2 8 2 8 0 6 4 4 4 4 8 6 6 4 5 2 3 8 7 4 9 3 0 3 5 8 9 0 7 2 9 6 2 9 0 4 9 1 5 6 0 4 4 0 7 7 2 3 9 0 7 1 3 8 1 0 5 1 5 8 5 9 3 0 7 9 6 0 8 6 6 7 0 1 7 2 4 2 7 1 2 1 8 8 3 9 9 8 7 9 7 9 0 8 7 9 2 2 7 4 9 2 1 9 0 1 6 9 9 7 2 0 8 8 8 0 9 3 7 7 6 6 5 7 2 7 3 3 3 0 0 1 0 5 3 3 6 7 8 8 1 2 2 0 2 3 5 4 2 1 8 0 9 7 5 1 2 5 4 5 4 0 5 9 4 7 5 2 2 4 3 5 2 5 8 4 9 0 7 7 1 1 6 7 0 5 5 6 0 1 3 6 0 4 8 3 9 5 8 6 4 4 6 7 0 6 3 2 4 4 1 5 7 2 2 1 5 5 3 9 7 5 3 6 9 7 8 1 7 9 7 7 8 4 6 1 7 4 0 6 4 9 5 5 1 4 9 2 9 0 8 6 2 5 6 9 3 2 1 9 7 8 4 6 8 6 2 2 4 8 2 8 3 9 7 2 2 4 1 3 7 5 6 5 7 0 5 6 0 5 7 4 9 0 2 6 1 4 0 7 9 7 2 9 6 8 6 5 2 4 1 4 5 3 5 1 0 0 4 7 4 8 2 1 6 6 3 7 0 4 8 4 4 0 3 1 9 9 8 9))
  (define (product lst)
    (if (empty? lst) 1 (* (car lst) (product (cdr lst)))))
  (define (max-product lst)
    (define (loop lst max)
      (if (< (length lst) 13)
          max
          (let ((prod (product (take lst 13))))
            (loop (cdr lst) (if (> prod max) prod max)))))
    (loop digits 0))
  (max-product digits))


;; Problem 9
;; Special Pythagorean triplet: a + b + c = 1000, find abc
(define (problem-9)
  (define (loop a b)
    (cond ((>= a 1000) 'not-found)
          ((>= b 1000) (loop (+ a 1) (+ a 2)))
          (else (let* ((c (- 1000 (+ a b)))
                       (check (= (+ (* a a) (* b b)) (* c c))))
                  (if check
                      (* a b c)
                      (loop a (+ b 1)))))))
  (loop 1 2))


;; Problem 10
;; Sum of all primes below two million
(define (problem-10)
  (define (is-prime? n)
    (define (loop d)
      (cond ((= n 2) #t)
            ((= (remainder n d) 0) #f)
            ((> (* d d) n) #t)
            (else (loop (+ d 1)))))
    (loop 2))
  (define (sum-primes i acc)
    (if (>= i 2000000)
        acc
        (sum-primes (+ i 1) (if (is-prime? i) (+ acc i) acc))))
  (sum-primes 2 0))


;; Example of running all problems
(begin
  (displayln (problem-1))
  (displayln (problem-2))
  (displayln (problem-3))
  (displayln (problem-4))
  (displayln (problem-5))
  (displayln (problem-6))
  (displayln (problem-7))
  (displayln (problem-8))
  (displayln (problem-9))
  (displayln (problem-10)))
