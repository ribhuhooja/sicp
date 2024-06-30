#lang sicp

;;2.17
(define (last-pair items)
  (if (null? (cdr items))
             (car items)
             (last-pair (cdr items))))

;;2.18
(define (reverse items)
  (if (null? items)
      nil
      (cons (last-pair items) (reverse (until-second-last items)))))

(define (until-second-last items)
  (if (null? (cdr items))
      nil
      (cons (car items) (until-second-last (cdr items)))))

;;skipping 2.19 -- Also, the above solution for 2.18 is probably pretty inefficient

;2.20
(define (same-parity x . items)
  (define (same-parity? x y)
    (= 0 (remainder (- x y) 2)))
  (define (collect items accd)
    (cond ((null? items) accd)
          ((same-parity? x (car items))
           (collect (cdr items) (cons (car items) accd)))
          (else (collect (cdr items) accd))))
  (collect items nil))

                                    
        
    
