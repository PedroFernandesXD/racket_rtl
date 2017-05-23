#lang racket

;; ripple-carry adder 

(define (gxor A B)
  (or (and A
           (not B))
      (and B
           (not A))))

(define (b+ A B C)
  (let* [(AxB (gxor A B))
         (A+B (gxor AxB C))]
    (values A+B
            (or (and AxB
                     C)
                (and A
                     B))) ))

(define (->tf n)
  (for/list [(x (~a (number->string n 2)
                    #:align 'right
                    #:left-pad-string "0"
                    #:width 4))]
    (char=? x #\1)))

(define (->01 xs)
  (list->string
   (for/list [ (x xs) ]
     (if x #\1 #\0)) ))

(define (adder p q)
  (let*-values [((a3 a2 a1 a0) (apply values p))
                ((b3 b2 b1 b0) (apply values q))
                ((s0 c0) (b+ a0 b0 #f))
                ((s1 c1) (b+ a1 b1 c0))
                ((s2 c2) (b+ a2 b2 c1))
                ((s3 c3) (b+ a3 b3 c2))]
    (list s3 s2 s1 s0)))

(define (negar xs)
  (adder (for/list [(x xs)]
           (not x))
         '(#f #f #f #t)))

(define (soma n1 n2)
  (let [(s (adder (->tf n1)
                  (->tf n2)))]
    (with-input-from-string (~a "#b" (->01 s)) read)))

(define (subtair n1 n2)
  (let [(s (adder (->tf n1)
                  (negar (->tf n2))))]
    (with-input-from-string (~a "#b"(->01 s)) read)))
