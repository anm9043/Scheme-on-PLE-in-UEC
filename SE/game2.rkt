(define *seed* 1)
(define (srand x)
    (set! *seed* x))
(define (irand)
    (set! *seed* (modulo (+ (* 69069 *seed*) 1) #x100000000))
    *seed*)
(define (random)
    (* (/ 1.0 #x100000000) (irand)))
(define (make-number n)
    (+ (modulo (quotient (irand) #x10000) n) 0))
(define (make-answer answer)
  (if (= (length answer) 4)
      answer
      (let ((num (make-number 10)))
        (if (member num answer)
            (make-answer answer)
            (make-answer (cons num answer))))))
(define (input-one-number)
  (let ((num (read)))
    (cond ((not (integer? num))
           (display "Please input integer (0 - 9)\n") #f)
          ((<= 0 num 9) num)
          (else (display "range error\n") #f))))
(define (input-four-numbers)
  (display "Please input four numbers\n> ")
  (let loop ((num-list '()))
    (if (= (length num-list) 4)
        (reverse num-list)
        (let ((num (input-one-number)))
          (cond ((not num)
                 (delete-input-data)
                 (input-four-numbers))
                ((member num num-list)
                 (display "Same number error\n")
                 (delete-input-data)
                 (input-four-number))
                (else (loop (cons num num-list))))))))
(define (delete-input-data)
  (let ((c (read-char)))
    (if (not (char=? #\newline c))
        (delete-input-data))))
(define (count-bulls answer data)
  (cond ((null? answer) 0)
        ((= (car answer)(car data))
         (+ 1 (count-bulls (cdr answer)(cdr data))))
        (else (count-bulls (cdr answer)(cdr data)))))
(define (count-same-number answer data)
  (cond ((null? answer) 0)
        ((member (car answer) data)
         (+ 1 (count-same-number (cdr answer) data)))
        (else (count-same-number (cdr answer) data))))
(define (display-gameover answer)
  (display "GameOver: ")
  (display answer)
  (newline))
(define (display-bulls-cows count answer data bulls)
  (display count)
  (display " : ")
  (display "bulls ")
  (display bulls)
  (display ", cows ")
  (display (- (count-same-number answer data) bulls))
  (newline))
(define (play answer)
  (let loop ((count 1))
    (let* ((data (input-four-numbers))
           (bulls (count-bulls answer data)))
      (display-bulls-cows count answer data bulls)
      (cond ((= bulls 4)
             (display "Congraturations!\n"))
            ((<= 10 count)
             (display-gameover answer))
            (else (loop (+ count 1)))))))