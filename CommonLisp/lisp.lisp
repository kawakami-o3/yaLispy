; yalisp

(defclass environment ()
  ((outer :accessor outer :initform '() :initarg :env)
   (dictionary :accessor dictionary :initform '() :initarg :dictionary)))

(defmethod put ((env environment) (name symbol) (fun function))
  (let ()
    (setf (dictionary env) (cons (list name fun) (dictionary env)))
    env
    ))

(defmethod find_env ((env environment) (name symbol))
  (let ((fun (find_sexp name (dictionary env))))
    (if fun
      env
      (find_env (outer env) name))))

(defun find_sexp (name lst)
  (if (= 0 (length lst))
    nil
    (let ((pair (car lst)))
      (if (string= name (car pair))
        (car (cdr pair))
        (find_sexp name (cdr lst))))))

(defun create_global_environment ()
  (let ((env (make-instance 'environment)))
    ;(put env '+ (lambda (x) (+ (car x) (car (cdr x)))))
    (put env '+ (lambda (x y) (+ x y)))
    env))

(defun eval_sexp (x env)
  (if (typep x 'symbol)
    (return-from eval_sexp '(env (find_sexp x (dictionary (find_env env x))))))
  (if (not (consp x))
    (return-from eval_sexp '(env x)))
  (case (car x)
    ("quote" "quote")
    ("if" "quote")
    ("set!" "quote")
    ("define" "quote")
    ("lambda" "quote")
    ("begin" "quote")
    (otherwise (let ((exps (mapcar (lambda (y) (eval_sexp y env)) x)))
                (if (string= (car (cdr exps)) "lambda")
                 '(nil env)
                 (apply (car exps) (cdr exps)))))))

#|
(defun add_space (str)
  (setq str (concatenate 'list str))
  (if (= 0 (length str))
    ""
    (let
      ((x (car str))
       (remains (add_space (cdr str))))
      (if (or (string= x "(") (string= x ")"))
        (format nil " ~a ~a" x remains)
        (format nil "~a~a" x remains)))))

(defun words (arg)
  (if (= 0 (length arg))
    '()
    (let ((str (format nil "~a " arg)))
      (let ((i 0) (len (length str)) (tmp "") (result '()))
        (loop for x in (concatenate 'list str)
              if (char= x #\Space) do
              (if (< 0 (length tmp))
                (progn
                  (setq result (cons tmp result))
                  (setq tmp "")))
              else do
              (setq tmp (format nil "~a~a" tmp x)))
        (reverse result)))))

(defun tokenize (str)
  (words (add_space str)))

(defun shift (lst)
  (let ((x (car lst)) (lst_new (cdr lst)))
        (setf (car lst) (car lst_new))
        (setf (cdr lst) (cdr lst_new))
        x))

(defun read_from (tokens)
  (if (= 0 (length tokens))
    (error "SyntaxError: unexpected EOF while reading"))
  (let ((token (shift tokens)))
    (if (string= "(" token)
      (let ((ret '()))
        (loop while (string/= ")" (car tokens)) do
              (setq ret (cons (read_from tokens) ret)))
        (reverse ret))
      (if (string= ")" token)
        (error "SyntaxError: unexpected )")
        token)))) ; TODO parse token
|#

(defun parse (str)
  ;(read_from (tokenize str)))
  (read-from-string str))

(defun repl ()
  (let ((env (create_global_environment)))
    (loop
      (format t "MyLisp> ")
      (setf s (read-line))
      (format t "-> ~a~%" (eval_sexp (parse s) env)))))

(repl)

