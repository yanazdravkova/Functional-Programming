#lang racket
;Задача 4. Да се напише функция maximize, която получава непразен списък от
;едноместни числови функции и връща нова едноместна числова функция на
;аргумент x, която връща стойността f(x) на тази фунция f от списъка, за която
;числото f(x) е най-голямо по абсолютна стойност.
;Примери:
;((maximize (list (λ (x) (- x 10)) (λ (x) (- x 5)))) 5) → -5
;((maximize (list (λ (x) (- x 10)) (λ(x) (- x 5)))) 9) → 4

;(get-element-index x xs) намира позицията на елемент x в списъка xs; ако има повече от 1 срещане на x, връща се индекса на първото такова
;ако х не е в списъка, връща се невалидна стойност -1
(define (get-element-index x xs)
  (define (helper curr-idx remain)
    (cond [(null? remain) -1]
          [(equal? x (car remain)) curr-idx];сравнение с equal?, за да работи функцията не само при списъци от числа
          [else (helper (+ curr-idx 1) (cdr remain))]))
  (helper 0 xs))
;(element-at-index idx xs) намира елемента на позиция idx; при невалидна позиция, връща се -1
(define (element-at-index idx xs)
  (define (helper curr-idx remain)
    (cond [(null? remain) -1]
          [(= idx curr-idx) (car remain)]
          [else (helper (+ curr-idx 1) (cdr remain))]))
  (helper 0 xs))
;(maximize fs) функцията от условието
(define (maximize fs)
  (λ(x)
    (let* ([ys (map (λ(f) (f x)) fs)];списък от стойностите на функциите, приложени над х
           [abs-ys (map abs ys)]; списък от абсолютните стойности на горния списък ys
           [abs-max (apply max abs-ys)];максимален елемент по абсолютна стойност
           [idx-abs-max (get-element-index abs-max abs-ys)]);позицията на максималния по абсолютна стойност елемент
      (element-at-index idx-abs-max ys))))

