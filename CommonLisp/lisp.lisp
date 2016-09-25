; yalisp

(defclass environment ()
  ((outer :accessor outer :initform '() :initarg :env)
   (dictionary :accessor dictionary :initform '() :initarg :dictionary)))

(defmethod define ((env environment) (name symbol) fun)
  (progn
    (setf (dictionary env) (cons (list name fun) (dictionary env)))
    env
    ))

(defmethod update ((env environment) (name symbol) fun)
  (let ((e (find_env env name)))
    (if e (define e namei fun) (define env name fun))))

(defmethod find_env (env (name symbol))
  (if env
    (let ((fun (find_sexp name (dictionary env))))
      (if fun
        env
        (find_env (outer env) name)))
    nil))

(defun find_sexp (name lst)
  (if (= 0 (length lst))
    nil
    (let ((pair (car lst)))
      (if (string= name (car pair))
        (car (cdr pair))
        (find_sexp name (cdr lst))))))


(defun create_environment (out_env vars args)
  (let ((env (make-instance 'environment)))
    (loop for k in vars for v in args do (define env k v))
    (setf (outer env) out_env)
    env))

(defun create_global_environment ()
  (let ((env (make-instance 'environment)))
    (define env '+ (lambda (x y) (+ x y)))
    (define env '= (lambda (x y) (= x y)))
    env))

(defun eval_sexp (x env)
  (if (typep x 'symbol)
    (return-from eval_sexp (list env (find_sexp x (dictionary (find_env env x))))))
  (if (not (consp x))
    (return-from eval_sexp (list env x)))
  (let ((fn (car x)) (arg (cdr x)))
    (case (car x)
      ('quote (list env (cadr x)))
      ('if (if (cadr (eval_sexp (cadr x) env))
             (eval_sexp (caddr x) env)
             (eval_sexp (cadddr x) env)))
      ('set! (let ((k (car arg)) (v (cadr (eval_sexp (cadr arg) env))))
               (list (update env k v) v)))
      ('define (let ((k (car arg)) (v (cadr (eval_sexp (cadr arg) env))))
               (list (define env k v) v)))
      ('lambda (list env x))
      (otherwise (let ((exps (mapcar (lambda (y) (cadr (eval_sexp y env))) x)))
                   ;(format t "eval_sexp.1> ~a~%" exps)
                   (if (equal 'function (type-of (car exps)))
                     (list env (apply (car exps) (cdr exps)))
                     (let ((vars (cadar exps)) (body (caddar exps)) (args (cdr exps)))
                         (eval_sexp body (create_environment env vars args)))))))))


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
        (loop for x in (concatenate 'list str) do
              (if (char= x #\Space)
                (if (< 0 (length tmp))
                  (progn
                    (setq result (cons tmp result))
                    (setq tmp "")))
                (setq tmp (format nil "~a~a" tmp x))))
        (reverse result)))))

(defun tokenize (str)
  (words (add_space str)))

(defun shift (lst)
  (let ((x (car lst)) (lst_new (cdr lst)))
        (setf (car lst) (car lst_new))
        (setf (cdr lst) (cdr lst_new))
        x))

(defun parse_atom (s)
  (let ((v (parse-integer s :junk-allowed t)))
    (if v v (intern (string-upcase s)))))

(defun read_from (tokens)
  (if (= 0 (length tokens))
    (error "SyntaxError: unexpected EOF while reading"))
  (let ((token (shift tokens)))
    (if (string= "(" token)
      (let ((ret '()))
        (loop while (string/= ")" (car tokens)) do
              (setq ret (cons (read_from tokens) ret)))
        (shift tokens)
        (reverse ret))
      (if (string= ")" token)
        (error "SyntaxError: unexpected )")
        (parse_atom token)))))



(defun parse (str)
  (format t ">> ~a~%" (tokenize str))
  (read_from (tokenize str)))
  ;(read-from-string str)) ;!!!

(defun repl ()
  (let ((env (create_global_environment)))
    (loop
      (format t "MyLisp> ")
      (setf s (read-line))
      (format t "-> ~a~%" (cadr (eval_sexp (parse s) env))))))

;(repl)


(defun do_test (commands)
  (let ((env (create_global_environment)))
    (loop for x in commands
          do
          (format t "> ~a~%" x)
;          (format t "parse> ~a~%" (parse x))
          (format t "==> ~a~%" (cadr (eval_sexp (parse x) env)))
          )))
;|#

(do_test
  (list
    "(+ 1 2)"
    "(set! a 2)"
    "(+ a 100)"
    "(define b 200)"
    "(+ a b)"
    "(define c (lambda (x) (+ x 1)))"
    "(c 1000)"
    ))

