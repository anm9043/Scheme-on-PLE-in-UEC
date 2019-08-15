(define fam '(A (B1 (C1)
                    (C2))
                (B2)
                (B3 (C3 (D1)
                        (D1)))))

(define fam1 '(a (b1 (c1)
                     (c2 (d1)
                         (d2 (e1)
                             (e2))
                         (d3)))
                 (b2)
                 (b3 (c3 (d4)
                         (d5))
                     (c4))
                 (b3 (c5 (d6 (e3 (f1)
                                 (f2))
                             (e4))))))

(define kakeizu
  (read (open-input-file "kakeizu")))

(define (sons tree) ;treeのトップの子要素のリストを返す
  (map car (cdr tree)))

(define (subtree num tree) ;treeのnum番目のサブツリーを返す
  (if (> num (length (cdr tree))) ;treeの子要素数がnumよりも少ない場合は
      #f                          ;#fを返す
      (let loop ((i num)(t tree))
        (if (< 1 i)
            (loop (- i 1)(cdr t))
            (car (cdr t))))))

(define get-depth
  (lambda (famtre depth)
    (cond ((or (null? famtre) (< depth 1)) '())
          ((pair? famtre) (map (lambda (t) (get-depth t (- depth 1))) famtre))
          (else (map car famtre)))))