(define (** n1 n2)
  (if (= n2 0) 1 (* n1 (** n1 (- n2 1)))))

(define (diff expression)
  (cond ((number? expression) 0)
        ((equal? expression 'x) 1)
        ((equal? '+ (car expression))
         (cons '+ (map diff (cdr expression))))
        ((equal? '- (car expression))
         (cons '- (map diff (cdr expression))))
        ((equal? '* (car expression))
         `(+ (* ,(cadr expression),(diff (caddr expression)))
             (* ,(diff (cadr expression)),(caddr expression))))
        ((equal? '** (car expression))
         `(* ,(caddr expression)
             (* ,(diff (cadr expression))
                (** ,(cadr expression),(- (caddr expression) 1)))))
        (else #f)))

;ここまで「その1」．ここから「その2」

(define (value expression num) ;expressionにnumを代入した値を求める
  ((eval `(lambda (x) ,expression)(interaction-environment)) num))

(define (derivative expression num) ;expressionのnumにおける微分係数を求める
  (let ((dfx (diff expression)))
    ((eval `(lambda (x) ,dfx)(interaction-environment)) num)))

(define (tangent expression num)
  (let ((derivative (derivative expression num))
        (value (value expression num)))
    `(+ (* ,derivative x),(- value (* derivative num)))))