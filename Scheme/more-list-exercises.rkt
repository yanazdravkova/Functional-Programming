#lang racket
;1.Да се дефинира функция maximum(l), която намира най-голямото число в списъка от числа l.
(define (maximum li)

  (define (helper curr-max remain)
    (cond
      [(null? remain) curr-max]
     [(< curr-max (car remain))(helper (car remain) (cdr remain))]
     [else (helper curr-max (cdr remain))]))
  (helper (car li) (cdr li)))
(require rackunit rackunit/text-ui)

(define maximum-tests
  (test-suite
    "Tests for maximum"

    (check = (maximum '(2)) 2)
    (check = (maximum '(5 3 5 5)) 5)
    (check = (maximum '(8 4 92 82 8 13)) 92)
    (check = (maximum '(8 4 82 12 31 133)) 133)))

(run-tests maximum-tests)


;2.Да се дефинира функция remove(l, x), която връща нов списък, в който е премахнато първото срещане на елемента x в списъка l.
;Например, (remove '(1 7 3 3 7) 7) връща '(1 3 3 7).
;(define (remove li x)
;  (define (helper flag remain)
;    (cond
;      [(= flag 1) remain]
;      [(not(= (car remain) x)) (helper 0 (cons (car remain) (helper 0 (cdr remain)))) ]
;      [(= (car remain) x) (helper 1 (cdr remain))]))
;  (helper 0 li))
;(remove (list 7 5 3) 5)

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
;remove
(define (remove li x)
  (define (helper clean rest)
    (if(= (car rest) x)
       (append (reverse clean) (cdr rest))
       (helper (cons (car rest) clean) (cdr rest))))
  (helper '() li))

                      
(require rackunit rackunit/text-ui)

(define remove-tests
  (test-suite
    "Tests for remove"

    (check-equal? (remove '(42) 42) '())
    (check-equal? (remove '(5 3 5 5) 5) '(3 5 5))
    (check-equal? (remove '(8 4 92 82 8 13) 82) '(8 4 92 8 13))
    (check-equal? (remove '(8 4 82 12 31 133) 133) '(8 4 82 12 31))))

(run-tests remove-tests)

;3. Да се дефинира функция selection-sort(l), която връща списъка l сортиран във възходящ ред чрез метода на пряката селекция. Добавете втори аргумент comp, който е бинарен оператор, който определя реда на сортировката.

;Например, (selection-sort '(1 2 3 4 5 6 7) >) връща '(7 6 5 4 3 2 1).


;Алгоритъмът работи по следния начин:
;
;Намира най-малкия елемент в списъка като сравнява първият елемент с всички останали
;Разменя го с елемента на първа позиция
;Повтаря горните две стъпки за всеки следващ елемент
;minimum
(define (minimum li)

  (define (helper curr-min remain)
    (cond
      [(null? remain) curr-min]
     [(> curr-min (car remain))(helper (car remain) (cdr remain))]
     [else (helper curr-min (cdr remain))]))
  (helper (car li) (cdr li)))
 ;selection-sort
(define (selection-sort li)
  (display li)
  (if (null? li)
      null
    (append (list (minimum li)) (selection-sort (remove li (minimum li))))))
;3. Да се дефинира функция selection-sort(l), която връща списъка l сортиран във възходящ ред
;чрез метода на пряката селекция. Добавете втори аргумент comp, който е бинарен оператор, който определя реда на сортировката.
;sort a list

(define (compare l comp)
  (define (comp-iter x l)
    (cond ((null? l) x)
          ((comp (car l) x) (comp-iter (car l) (remove l x)))
          (else ( comp-iter x (remove l (car l))))))
  (comp-iter (car l) l))

(define (selection_sort l cmp)
  (if(null? l) '()
     (cons (compare l cmp) (selection_sort (remove l (compare l cmp)) cmp))))
;4.Да се дефинира функция partition(p, l), която връща списък от два подсписъка, където:
;първият съдържа всички елементи на l, които удовлетворяват предиката p.
;вторият съдържа всички останали елементи на l.
(define (partition p li)
  (append (list (filter p li)) (list (filter (λ(x) (not (p x))) li))))

(require rackunit rackunit/text-ui)

(define partition-tests
  (test-suite
    "Tests for partition"

    (check-equal? (partition even? '(1 2 3 4 5 6 7)) '((2 4 6) (1 3 5 7)))
    (check-equal? (partition odd? '(1 3 3 7 42)) '((1 3 3 7) (42)))
    (check-equal? (partition odd? '(3)) '((3) ()))
    (check-equal? (partition even? '()) '(() ()))
    (check-equal? (partition (lambda (x) (< x 4)) '(1 2 3 4 5 6 7))
                  '((1 2 3) (4 5 6 7)))))

(run-tests partition-tests)
;5. Да се дефинира функция flatten(l), която приема списък от атоми (числа) и списъци с атоми l и връща списък с всички атоми.
;Например, (flatten '((1 2) 3 (4 5) (6 7))) връща '(1 2 3 4 5 6 7).
(define (flatten1 l)
  (cond((null? l) '())
       ((pair? (car l)) (flatten1(append (car l) (cdr l))))
         (else (cons (car l) (flatten1 (cdr l))))))
;!проблем прави това, което вече е списък, да е списък в списък
(define (flatten li)
 (apply append (map (λ(x) (if (list? x)
                              x
                              (list x))) li)))
;(define flatten-tests
;  (test-suite
;    "Tests for flatten"
;
;    (check-equal? (flatten '((1 2) (3 4) (5 6))) '(1 2 3 4 5 6))
;    ;(check-equal? (flatten '((1 2) 3 (4 5) (6 7))) '(1 2 3 4 5 6 7))
;    (check-equal? (flatten '(5 3 5 5) ) '(5 3 5 5))
;    (check-equal? (flatten '(5 () 3 () 5 () 5) ) '(5 3 5 5))))
;
;(run-tests flatten-tests)

;6.Да се дефинира функция map-deep(f, l), която прилага функцията f върху всеки атом от всеки вложен списък в списъка l.
;Например, (map-deep square '((1 2 (3 4)) 5)) връща '((1 4 (9 16)) 25).
(define (map-deep f li)
  (cond
    [(null? li) null]
    [(pair? (car li)) (cons (map-deep f (car li)) (map-deep f (cdr li)))]
    [else (cons (f (car li)) (map-deep f (cdr li)))]))
;7. Да се дефинира функция zip(a, b), която чифтосва списъците a и b поелементно и връща списък от наредени двойки (ai, bi),
;където ai и bi са съответно i-тите елементи в a и b. Новият списък е с дължината на по-малкия от двата подадени списъка.
(define (zip xs ys)
  (cond
    [(null? xs) null]
    [(null? ys) null]
    [else (append (list (append (list (car xs)) (list (car ys)))) (zip (cdr xs) (cdr ys)))]))

;(require rackunit rackunit/text-ui)
;
;(define zip-tests
;  (test-suite
;    "Tests for zip"
;
;    (check-equal? (zip '(() (6 6 6))) '())
;    (check-equal? (zip '((1 1 1) (2 2))) '((1 2) (1 2)))
;    (check-equal? (zip '((1 3 5) (2 4 6))) '((1 2) (3 4) (5 6)))
;    (check-equal? (zip '((1 3 5 7 9) (2 4 6))) '((1 2) (3 4) (5 6)))))
;
;(run-tests zip-tests)
;8. Да се дефинира функция remove-duplicates(l), която премахва всички дубликати в списъка l и остава само уникалните елементи.
;Редът е без значение. Например, (remove-duplicates '(4 3 3 2 5 2)) връща '(4 3 2 5).

(define (remove-duplicates xs)
(define (helper result remain)
  (cond
    [(null? remain) (reverse result)]
    [(member (car remain) result) (helper result(cdr remain))]
    [else (helper (cons(car remain) result) (cdr remain))]))

  (helper null xs)
  )

   