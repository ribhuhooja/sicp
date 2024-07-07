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
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

;a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;b
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((struc (branch-structure branch)))
    (if (not (pair? struc))
        struc
        (total-weight struc))))

;c
(define (is-balanced? mobile)
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (is-balanced-branch? (left-branch mobile))
       (is-balanced-branch? (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (is-balanced-branch? branch)
  (let ((struc (branch-structure branch)))
    (if (not (pair? struc))
        true
        (is-balanced? struc))))
;2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (map-square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (map-square-tree x)
             (* x x)))
       tree))

;2.31
(define (square x) (* x x))

(define (tree-map func tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map func (car tree))
                    (tree-map func (cdr tree))))))

;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;--------------------- thus ends section 2.2.2 ----------------------------

;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (seqmap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (seqappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (seqlength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node)
                             1))
                       tree)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product v r)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; property -> associativity

;2.39
(define (acc-reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))

(define (foldl-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;2.40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime? n)
  (define max (floor (sqrt n)))
  (define (check-prime k)
    (cond ((> k max) true)
          ((= (remainder n k) 0) false)
          (else (check-prime (+ k 1)))))
  (if (= n 1) false (check-prime 2)))

(define (prime-sum-pairs n)
  (define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

;2.41
(define (triples-that-sum-to s n)
  (filter (lambda (triple) (= s (+ (car triple)
                                   (cadr triple)
                                   (caddr triple))))
          (enumerate-triples n)))

(define (enumerate-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

     
