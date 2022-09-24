#lang racket

; disclaimer.
; in this problem set i use
; lists instead of pairs for simplicity,
; if you see '((a b) (c d)) this means '((a . b) (c . d))

; Exercise 1
; 1.a
(define (replicate num val)
  (cond
    [(<= num 0) empty]
    [else (cons val (replicate (- num 1) val))]))

; 1.b
(define (split lst size)
  (define (split-helper left right cur size)
    (cond
      [(= cur size) (list (reverse left) right)]
      [else (split-helper (cons (first right) left) (rest right) (+ 1 cur) size)]))
  (cond
    [(<= size 0) (list empty lst)]
    [(>= size (length lst)) (list lst empty)]
    [else (split-helper empty lst 0 size)]))

; 1.c
(define (chunks lst size)
  (cond
    [(<= (length lst) size) (list lst)]
    [else (append (list (first (split lst size))) (chunks (first (rest (split lst size))) size))]))

; 1.d
(define (windows lst size)
  (cond
    [(<= (length lst) size) (list lst)]
    [else (append (list (first (split lst size))) (windows (rest lst) size))]))

; Exercise 2
; 2.a
(define (pairs-one lst)
  (foldl (lambda (elem res) (append res (list (list (first lst) elem)))) empty (rest lst)))

(define (pairs lst)
  (rest (foldl
   (lambda(elem res) (cons (+ 1 (first res)) (append (rest res) (pairs-one (list-tail lst (first res))))))
   '(0) lst)))

; 2.b
(define (splits lst)
  (rest (foldl
   (lambda(elem res) (cons (+ 1 (first res)) (append (rest res) (list (split lst (first res))))))
   '(1) lst)))

; 2.c
(define (prod lst)
  (* (first lst) (second lst)))

(define (max-product lst)
  (foldl
   (lambda(elem max-pair) (if (> (prod elem) (prod max-pair)) elem max-pair))
   (list (first lst) (second lst))
   (pairs lst)))

; 2.d
(define (max-binary-op op lst)
  (foldl
   (lambda(elem max-pair) (if (> (op (first elem) (second elem)) (op (first max-pair) (second max-pair)))
                              elem max-pair))
   (list (first lst) (second lst))
   (pairs lst)))

; Exercise 3
; 3.a
(define (max lst)
  (foldl
   (λ (elem mx) (if (> elem mx) elem mx))
   0
   lst))

; 3.b
(define (second-max lst)
  (car (foldl
   (λ (elem mp) (if (and (> elem (car mp)) (< elem (cdr mp)))
                    (cons elem (cdr mp))
                    mp))
   (cons 0 (max lst))
   lst)))

; 3.c
(define (top-3 lst)
  (foldl
   (λ (elem t3) (if (and (> elem (first t3)) (< elem (second t3)))
                    (cons elem (rest t3))
                    t3))
   (list 0 (second-max lst) (max lst))
   lst))

; 3.d
(define (group lst)
  (rest (foldl
   (λ (elem res) (if (equal? elem (first (last res)))
                     (append (take res (- (length res) 1)) (list (append (last res) (list elem))))
                     (append res (list (list elem)))))
   '((ඞ))
   lst))) 

; 3.e
(define (cumulative-sum prefix lst)
  (foldl
   (λ (elem sum) (+ elem sum))
   0
   (first (split lst prefix))))

(define (cumulative-sums lst)
  (rest (foldl
   (λ (elem res) (cons (+ 1 (first res)) (append (rest res) (list (cumulative-sum (first res) lst)))))
   (list 1 (cumulative-sum 0 lst))
   lst))) 
