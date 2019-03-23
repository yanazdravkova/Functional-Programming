#lang racket
;Задача 3. Run-length encoding (RLE) е метод за компресия на списъци, при който
;всяка поредица от еднакви елементи се представя като списък от елемента и
;броя на повторенията му. Да се напише функция encode, която компресира
;даден списък.
;Примери:
;(encode '(a a a a b c c a a d e e e e)) → ((a 4) (b 1) (c 2) (a 2) (d 1)(e 4))
;(encode '(m i s s i s s i p p i)) → ((m 1) (i 1) (s 2) (i 1) (s 2) (i 1) (p 2) (i 1))

;(count-occure x xs) брои срещанията на елемента x в списъка xs, в който x е първият елемент 
(define (count-occure x xs)
  (cond [(= (length xs) 1) 1]
        [(not (equal? x (cadr xs)))1]
     [else (+ 1 (count-occure x (cdr xs)))]))
;(iter n xs) итерира n-пъти и отрязва списъка до там
(define (iter n xs)
  (if(= n 1)
     (cdr xs)
     (iter (- n 1) (cdr xs))))
;(encode xs) функцията от условието 
(define (encode xs)
  (if(null? xs)
     null
  (append (list (cons (car xs)
                        (list (count-occure (car xs) xs))))
          (encode (iter (count-occure (car xs) xs) xs)))))