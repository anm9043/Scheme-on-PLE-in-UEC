(define tree1 '(1 (2 (3 4)) 6 (7 8 9)))
(define mymap
  (lambda (fn ls)
    (if (null? ls)
        '()
        (cons (fn (car ls))(mymap fn (cdr ls)))
        )
    )
  )

(define cb (lambda(x)(* x x)))

(define map-tree
  (lambda(fn tree)
    (cond ((null? tree) '())
          ( (pair? tree)
            (cons (map-tree fn (car tree))(map-tree fn (cdr tree)))
            )
          (else (fn tree))
          )
    )
  )

(define map-tree2
  (lambda(fn tree)
    (cond ((null? tree) '())
          ((pair? tree)(lambda(t) (map-tree2 fn t)))
          (else (fn tree))
          )
    )
  )