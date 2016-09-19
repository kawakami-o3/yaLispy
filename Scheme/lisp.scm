

(define (shift lst)
  (let ((lst_new (cdr lst)))
    (set! (car lst) (car lst_new))
    (set! (cdr lst) (cdr lst_new))
    lst
    ))

(define (repl)
  (let ((buf (read)))
    (cond ((not (eof-object? buf))
      (print buf)
      (repl)))))

(repl)
