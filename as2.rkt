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

(define (nos tree) ;the Number Of Sons
  (length (sons tree)))

(define (make-grandsons-tree tree);孫を子とみなしたような木を作る
  (let ((result (list (car tree))))
    (let loop ((i 1))
      (if (<= i (nos tree))
          (begin (set! result (append result (cdr (subtree i tree))))
                 (loop (+ i 1)))
          #f))
    result))

(define (get-depth tree depth)
  (let ((result '()))
    (cond ((<= depth 0)(set! result (list (car tree))))
          ((= depth 1)(set! result (append result (sons tree))))
          ((= depth 2) ;depth >= 2の時
           (let loop ((t tree)(i 1))
             (if (<= i (nos t))
                 (begin (set! result (append result (sons (subtree i t))))
                        (loop t (+ i 1)))
                 #f)))
          (else (set! result (append result (get-depth (make-grandsons-tree tree) (- depth 1))))))
    result))

;ここまで「その1」．ここから「その2」
(define (get-cousin tree item)
  (let loop ((i 0))
    (let ((l (get-depth tree i)))
      (if (null? l)
          #f
          (if (member item l)
              l
              (loop (+ i 1)))))))

;ここまで「その2」．ここから「その3」

(define (get-generation tree item)
  (let loop ((i 0))
    (let ((l (get-depth tree i)))
      (if (null? l)
          #f
          (if (member item l)
              i
              (loop (+ i 1)))))))
(define (get-descendant-tree tree num) ;make-grandsons-treeをnum回繰り返す
  (let loop ((t tree)(i num))
    (if (= i 0)
        t
        (loop (make-grandsons-tree t)(- i 1)))))
        
  

(define (get-father tree item)
  (let ((generation (get-generation tree item)))
    (if (not generation)
        #f
        (cond ((= generation 0) '())
              ((= generation 1) (car tree))
              (else
               (let* ((uncles-tree (get-descendant-tree tree (- generation 2)))
                      (uncles (nos uncles-tree)))
                 (let loop ((i 1))
                   (let ((t (subtree i uncles-tree)))
                     (if (member item (sons t))
                         (car t)
                         (if (< i uncles)
                             (loop (+ i 1))
                             #f))))))))))

(define (get-path tree item)
  (let ((generation (get-generation tree item)))
    (if (not generation)
        #f
        (let ((result (list item)))
          (let loop ((g generation)
                     (i item))
            (if (<= g 0)
                result
                (begin (set! result (cons (get-father tree i) result))
                       (loop (- g 1) (get-father tree i)))))))))
        