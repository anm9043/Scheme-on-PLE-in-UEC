(define tree1 '(1 (2 (3 4)) 6 (7 8 9)))
(define tree2 '((1 2) 3 (4 (5 (6 7)) 8) (9 10 (11 12 (13 14 15)))))
(define cb (lambda(x)(* x x)))

(define map-tree
  (lambda(fn tree)
    (cond ((null? tree) '())
          ( (pair? tree)
            (cons (map-tree fn (car tree))(map-tree fn (cdr tree))))
          (else (fn tree)))))

(define map-tree2
  (lambda(fn tree)
    (cond ((null? tree) '())
          ((pair? tree) (map (lambda (t) (map-tree2 fn t)) tree))
          (else (fn tree)))))