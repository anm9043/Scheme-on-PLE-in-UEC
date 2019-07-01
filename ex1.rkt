(define numlist
  (lambda(x)
        (if (= x 1)
            '(1)
            (cons x (numlist(- x 1)))
            )
        )
  )

(define myappend
  (lambda(l) (apply append l))
  )

(define times
  (lambda(x l)
    (map (lambda(n)(* x n)) l)
    )
  )

(define average
   (lambda (data) (/ (apply + data) (length data)))
  )

(define rms
  (lambda (l)
    (sqrt (/ (apply + (map (lambda(n)(* n n)) l)) (length l)))
    )
  )
