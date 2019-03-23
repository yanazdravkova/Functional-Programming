#lang racket
;Задача 2. Да се напише функция sum-numbers, която приема един аргумент –
;символен низ и връща сумата на всички числа в него.
;Примери:
;(sum-numbers "a123b2c56") → 181
;(sum-numbers "a1b2c3") → 6


;(from-ascii c) превръща ascii номер в цифрата; работи само за цифри
(define (from-ascii c)
  (cond [(= c 48) 0]
        [(= c 49) 1]
        [(= c 50) 2]
        [(= c 51) 3]
        [(= c 52) 4]
        [(= c 53) 5]
        [(= c 54) 6]
        [(= c 55) 7]
        [(= c 56) 8]
        [(= c 57) 9]))
;1.(my-cut xs) взима списък с начален елемент символ = цифра(пример: #\7) и връща списък до последния пореден символ = цифра  '(#\1 #\2 #\3 #\a #\7) -> '(#\1 #\2 #\3)
(define (my-cut xs)
  (cons (car xs)
        (cond [ (= (length xs) 1) null]
              [(not(char-numeric? (cadr xs))) null]
              [else (my-cut (cdr xs))])))

;2.(char-to-digit xs) превръща списък от типа `(#\2 #\3) -> `(2 3)
(define (char-to-digit xs)
  (let [(ys (map char->integer xs))]
    (map from-ascii ys)))

;3.(list-to-number xs) превръща списък от цифри в число `(2 3) -> 23
(define (list-to-number xs)
  (let[(n (length xs))]
    (if(= n 1)
       (car xs)
       (+ (* (car xs) (expt 10 (- n 1)))
          (list-to-number (cdr xs))))))
;(take-num xs)- обединение на функции 1 2 3, т.е. взимаме списък, започващ със символ=цифра,
;режем го до последният символ=цифра, превръщаме го в списък, в който символите
;са с реалните си цифрени стойности, а не от ascii-то и накрая превръщаме получения списък от цифри в число
(define (take-num xs)
  (list-to-number (char-to-digit (my-cut xs))))

;(sum-numbers str) функцията от условието
(define (sum-numbers str)
  (let ([xs (string->list str)])
    (cond [(null? xs) 0]
          [(char-numeric? (car xs)) (+ (take-num xs)
                                       (sum-numbers (substring str (length (my-cut xs)) (length xs))))]
          [else (sum-numbers (substring str 1 (length xs)))])))
