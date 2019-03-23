#lang racket
;1. Да се дефинира функция run-length-encode(l), която кодира списъка l в асоциативен списък - списък от
;наредени двойки '(<ключ> . <стойност>), където <ключ>-ът e пореден елемент от списъка l, а <стойност>-та е колко пъти се повтаря елемента последователно.
;Например, (run-length-encode '(8 7 7 2 2 2 2 3 3 4)) връща асоциативния списък '((8 . 1) (7 . 2) (2 . 4) (3 . 2) (4 . 1)).
(define (count-occure x li);ще подаваме списък, в който x е първия елемент 
  (cond [(= (length li) 1) 1]
        [(not (equal? x (cadr li)))1]
     [else (+ 1 (count-occure x (cdr li)))]))

(define (iter n li);итерира n-пъти и отрязва списъка до там
  (if(= n 1)
     (cdr li)
     (iter (- n 1) (cdr li))))

(define (run-length-encode li)
  (if(null? li)
     null
  (append (list (cons (car li) (count-occure (car li) li))) (run-length-encode (iter (count-occure (car li) li) li)))))

;2. Да се дефинира функция run-length-decode(code), която възстановява списъка, който е кодиран чрез run-length-encode от предната задача
;в асоциативния списък code.
;Например, (run-length-decode '((1 . 2) (3 . 4) (5 . 2))) връща '(1 1 3 3 3 3 5 5).
;прави списък от el повторен n пъти
(define (repeat el n)
  (if(= n 1)
     (list el)
     (append (list el) (repeat el (- n 1)))))

(define (f x)
  (repeat (car x) (cdr x)))

(define (run-length-decode code)
 (apply append (map f code)))

;3. Да се дефинира функция histogram(l), която връща хистограма на срещанията на всички елементи в l под формата на асоциативен списък.
;Например, (histogram '(8 7 1 7 8 2 2 8 2 7 8 1)) връща асоциативния списък '((8 . 4) (7 . 3) (1 . 2) (2 . 3)).


(define (remove-duplicates xs)
(define (helper result remain)
  (cond
    [(null? remain) (reverse result)]
    [(member (car remain) result) (helper result(cdr remain))]
    [else (helper (cons(car remain) result) (cdr remain))]))

  (helper null xs)
  )

(define (count x li)
  (length (filter (λ(y) (equal? x y)) li)))
(define (magic li)
  (map(λ(x) (cons x (count x li))) li))
(define (histogram li)
  (remove-duplicates (magic li)))

;4. Да се дефинира функция group-by(f, l), която връща асоциативен списък, в който ключовете са стойностите на функцията f
;след прилагането ѝ върху елементи от списъка l, а срещу ключовете стои списък от елементите, за които функцията f дава стойността от ключа.
;Например, (group-by (lambda (x) (remainder x 3)) '(0 1 2 3 4 5 6 7 8)) връща асоциативния списък '((0 0 3 6) (1 1 4 7) (2 2 5 8)).

(define (foo f li)
  (map (λ(x) (cons (f x) 11)) li))
 ;(map (λ(x) (cons (f x) (map (λ(y) (equal? (f y) (f x))) li))) li))


