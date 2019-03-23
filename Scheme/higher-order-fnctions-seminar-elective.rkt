#lang racket

;help functions
;count-digits
(define (count-digits n)
  (if(< n 10)
        1
      (+ 1 (count-digits (/ n 10))))
)
;take-first-k-digits
(define (take-first-k-digits n k)
  (quotient n (expt 10 (- (count-digits n) k))))
;take-last-k-digits
(define (take-last-k-digits n k)
  (remainder n (expt 10 k)))
;reverse
(define (reverse-it n)
   (
    if(= (count-digits n) 1)
      n
      (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1)))
         (reverse-it (/(- n (remainder n 10)) 10)))))
;(define square (λ (x) (* x x)))
(define (square x) (* x x))
(define 2+ (λ (x) (+ 2 x)))
(define 1+ (λ (x) (+ 1 x)))
;3. Pascal triangle
(define (pascal idx row)
  (cond
    [(= idx 1) 1]
    [(= idx row) 1]
    [else (+ (pascal(- idx 1) (- row 1)) (pascal idx (- row 1)))]))
;4a. add next-a to accumulate 
(define (accumulate combiner null-value term a next-a b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next-a a)
                            next-a
                            b))))
;4b. accumulate-iter
(define (accumulate-iter combiner null-value term a next-a b)
  (define (helper result remain)
    (if (= a b )
        result
        (helper (combiner result (term remain))
                (next-a remain))))
  (helper null-value a))


;9.Напишете функция double, която приема функция на един аргумент и връща функция, която прилага подадената функция два пъти.
;Например, ако имаме функцията inc,която добавя 1 към своя аргумент ((define (inc x) (+ x 1)),
;то (double inc) е функция, която добавя 2 към своя аргумент.
(define (inc x) (+ x 1))
(define (double f)
  (λ (x) (f (f x))))
;10.Нека f и g са функции на един аргумент. Композицията f ∘ g е функцията x ↦ f(g(x)).
;Напишете функция compose(f, g), която връща композицията f ∘ g.
(define (compose f g)
  (λ(x) (f(g x))))
;11. Нека f е функция на един аргумент и n е цяло неотрицателно число. Дефинираме n-тото прилагане на функцията f да бъде функцията, дефинирана по следния начин:
;f0(x) = x
;fn(x) = f(fn-1(x))
;Напишете функция repeated(f, n), която връща n-тото прилагане на f

(define (repeated f n)
  (λ (x) (if (= n 0)
             x
             (f (repeated f (- n 1))))))
