#lang racket
;1.Да се дефинира функция dimensions(m), която връща наредена двойка с броя редове и броя колони на матрицата m.
;Например, (dimensions '((1 2 3) (4 5 6))) връща '(2 . 3).
(define (dimensions m)
  (cons (length m) (length (car m))))
;2. Да се дефинира функция reverse-columns(m), която обръща реда на колоните в матрицата m.
;Например, (reverse-columns '((1 2 3) (4 5 6) (7 8 9))) връща '((3 2 1) (6 5 4) (9 8 7)).
(define (reverse-columns m)
  (map reverse m))
;3. Да се дефинира функция nth-column(m, n), която връща списък с елементите на n-тата поред колона от матрицата m.
;Например, (nth-column '((1 2 3) (4 5 6) (7 8 9)) 2) връща '(2 5 8).
;list-ref
(define (nth-column m n)
  (map (λ(x) (list-ref x (- n 1))) m));n-1 щото list-ref брои от 0
;4. Да се дефинира функция main-diagonal(m), която връща списък с елементите в главния диагонал на матрицата m.
;Например, (main-diagonal '((1 2 3) (4 5 6) (7 8 9)) 2) връща '(1 5 9).
(define (main-diagonal m)
  (define (helper k result li)
    (display result)
    (if(null? li)
       result
       (helper (+ k 1) (append result (list (list-ref (car li) k))) (cdr li))))
  (helper 0 null m))
;5. Да се дефинира функция transpose(m), която връща транспонираната матрица на матрицата m.
;Например, (transpose '((1 2 3) (4 5 6))) връща '((1 4) (2 5) (3 6)).
(define (transpose m)
  (define (helper k)
    (if (= k (+ (length m) 1))
        null
        (append (list (map (λ(x) (list-ref x k)) m)) (helper (+ k 1)))))
  (helper 0))
;6. Да се дефинира предикат for-all-columns?(m, p), който проверява дали за всяка колона в матрицата m е изпълнен предикатът p.
(define (ev li)
  (andmap even? li)) 
(define (for-all-columns? m p)
  (andmap p (transpose m)))
;7. Да се дефинира предикат prime-in-each-column?(m), който проверява дали във всяка колона в матрицата m има просто число.
;Например, (prime-in-each-column? '((2 2 3) (4 5 6))) e #f.
;Но (prime-in-each-column? '((17 2 16) (4 5 3))) e #t.
;prime? от SICP book
(define (square x)
   (* x x))
(define (smallest-divisor n)
   (find-divisor n 2) ;starting the chek from a divisore qualling 2
   )
(define (find-divisor n test-divisor)
   (
    cond ((> (square test-divisor) n) n); if n is not prime then it has a divisor smaller than sqrt(n)
         ((divides? test-divisor n) test-divisor); have found a divisor and return it
         (else (find-divisor n (+ 1 test-divisor))); continue the search
    )
   )
(define (divides? a b)
   (=(remainder b a) 0);mind the order
   )
(define (prime? n)
   (= (smallest-divisor n) n)
   )
(define (prime-in-each-column? m)
  (ormap(λ(x)
        (filter prime? x)) (transpose m)))