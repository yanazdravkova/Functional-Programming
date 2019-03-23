#lang racket
;1. Да се дефинира функция length(l), която намира броя на елементите на списъка l.
(define (length li)
  (if (null? li)
      0
      (+ 1 (length (cdr li)))))
;2. Да се дефинира функция sum(l), която намира сумата на елементите на списъка l.
(define (sum li)
  (if (null? li)
      0
      (+ (car li) (sum (cdr li)))))
;3. Да се дефинира функция member?(l, x), която проверява дали x е елемент на списъка l.
(define (my-member? li x)
  (cond [(null? li) #f]
        [(= (car li) x) #t]
        [else (my-member? (cdr li) x)]))
;4. Да се дефинира функция last(l), която връща последния елемент на списъка l.
(define (last li)
  (if (= (length li) 1)
      (car li)
      (last (cdr li))))
;5. Да се дефинира функция nth(l, n), която връща елемента на позиция n, броейки от 0, в списъка l.
(define (nth li n)
  (if (= n 0)
      (car li)
      (nth (cdr li) (- n 1))))
;6. Да се дефинира функция scale(l, x), която връща списък с елементите на списъка l, умножени по числото x.
(define (scale x li)
  (if (null? li)
      li ;'() <-> null
      (cons(* (car li) x) (scale x (cdr li)))))
;7. Да се дефинира функция reverse(l), която връща списък, чиито елементи са елементите на списъка l в обратен ред.

(define (reverse li)
  (if (= (length li) 0)
      null
      (append  (reverse (cdr li)) (list (car li)))))
;8. Да се дефинира функция add-last(l, x), коята добавя елемент x на края на списъка l.
(define (add-last li x)
  (define (helper remain)
    (if(null? remain)
       (cons x null)
       (cons (car remain) (helper(cdr remain)))))
  (helper li))
;9. Да се дефинира функция append(l1, l2), която конкатенира списъците l1 и l2
(define (append l1 l2)
  (if(null? l2)
     l1
     (append (add-last l1 (car l2)) (cdr l2))))
;10. Да се дефинира функция map(l, f), която прилага функцията f върху всеки елемент на списъка l.
(define (my-map li f)
  (if(null? li)
     null
     (cons (f (car li)) (my-map (cdr li) f))))
;11. Да се дефинира функция filter(l, p), която връща списък с елементите на списъка l, които удовлетворяват предиката p.
(define (my-filter li p)
  (if(null? li)
     li
     (if(p (car li))
        (cons (car li) (my-filter (cdr li) p))
        (my-filter(cdr li) p))))
;12. Напишете процедурата accumulate, така че да работи със списъци вместо интервали от числа.
(define (accumulate-list combiner null-value li term)
  (if(null? li)
     null-value
     (combiner (term (car li))
               (accumulate-list combiner null-value (cdr li) term))))
