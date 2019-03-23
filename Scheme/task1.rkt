#lang racket
;Задача 1. Да се напише функция (convert x k n), която получава цяло число x ≥ 0,
;записано в k-ична позиционна бройна система, и връща съответното му число в
;n-ична бройна система, където 2 ≤ k, n ≤ 10.
;Забележка. Не е позволена употребата на символни низове, както и на всякакви
;вградени функции, които получават директно резултата.
;Примери:
;(convert 123 10 2) → 1111011
;(convert 173 8 10) → 123

;(digit-count x) дава броя на цифрите в число
(define (digit-count x)
  (if (< x 10)
      1
      (+ 1 (digit-count (quotient x 10)))))
;(take-1st-digit x) взима първата цифра на число
(define (take-1st-digit x)
  (quotient x (expt 10 (- (digit-count x) 1))))
;(to-decimal x k) превръща числото х от к-ична в десетична бройна система
(define (to-decimal x k) 
 (if (< x 10)
     x
     (+ (* (take-1st-digit x) (expt k (- (digit-count x) 1)))
        (to-decimal (remainder x (expt 10 (- (digit-count x) 1))) k))))
;(to-nbase x n) превръща число от десетична в n-ична бройна система
(define (to-nbase x n)
 (define (helper num)
  (if (zero? num)
      null
  (append (list (remainder num n)) (to-nbase (quotient num n) n))))
 (helper x))
;(list-to-num xs) превръща списък, съдържащ цифрите на число, в самото число
(define (list-to-num xs)
  (let ([length (length xs)])
     (if(= length 1)
        (* (car xs) 1)
        (+ (* (car xs) (expt 10 (- length 1))) (list-to-num (cdr xs))))))
;(convert x k n) функцията от условието
(define (convert x k n)
   (cond [(= x 0) 0]
         [(= k n) x]
         [(= k 10) (list-to-num (reverse (to-nbase x n)))]
         [(= n 10) (to-decimal x k)]
         [else (list-to-num (reverse (to-nbase (to-decimal x k) n)))]))
