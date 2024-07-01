#lang sicp

;;2.17
(define (last-pair items)
  (if (null? (cdr items))
             (car items)
             (last-pair (cdr items))))

;;2.18
(define (reverse items)
  (define (collect items accd)
    (if (null? items)
        accd
        (collect (cdr items) (cons (car items) accd))))
  (collect items nil))

;;skipping 2.19 -- Also, the above solution for 2.18 is probably pretty inefficient

;2.20
(define (same-parity x . items)
  (define (same-parity? x y)
    (= 0 (remainder (- x y) 2)))
  (define (collect items)
    (cond ((null? items) nil)
          ((same-parity? x (car items))
           (cons (car items) (collect (cdr items))))
          (else (collect (cdr items)))))
  (cons x (collect items)))

;2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (map-square-list items)
  (map (lambda (x) (* x x)) items))

;2.22
(define (foreach proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (foreach proc (cdr items)))))

;------------------------ thus ends 2.2.1 ----------------------------
;2.24 - pointless
;2.25 - also pointless
;2.26 - also also pointless
;2.27
(define (deep-reverse items)
  (define (collect items accd)
    (cond ((null? items) accd)
          ((not (pair? items))
           (if (null? accd)
               items
               (cons items accd)))
          (else (collect (cdr items)
                         (cons (deep-reverse (car items)) accd)))))
  (collect items nil))

;2.28
(define (fringe tree)
  (define (collect tree accd)
    (cond ((null? tree) accd)
          ((not (pair? tree)) (append accd (cons tree nil)))
          (else (collect (cdr tree) (collect (car tree) accd)))))
  (collect tree nil))
          
;2.29


        
