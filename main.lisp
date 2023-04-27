;; environment
(defparameter *everything* (make-hash-table))
(setf (gethash 'GOD *everything*) (make-hash-table))

;; eval

(defun eval-list (lst &optional caller)
  (if (null lst)
      nil
    (cons (eval. (first lst) caller) (eval-list (rest lst) caller))))

(defun cond-eval (clauses &optional caller)
  (cond ((eval. (first (first clauses)) caller) (eval. (second (first clauses)) caller))
        (t (cond-eval (rest clauses) caller))))

(defun apply. (object method &rest args)
  (apply. (get object method) caller args))
;((eq (first expr) 'apply) (apply (eval (second expr) caller) (eval (third expr) caller) (eval-list (cdr (cdr (cdr expr)))) caller))

(defun eval. (expr caller)
  (cond ((atom expr) expr)
        ((eq (first expr) 'quote) (second expr))
        ((eq (first expr) 'atom) (atom (eval. (second expr) caller)))
        ((eq (first expr) 'eq) (eq (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'first) (car (eval. (second expr) caller)))
        ((eq (first expr) 'rest) (cdr (eval. (second expr) caller)))
        ((eq (first expr) 'cons) (cons (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'eval) (eval. (second expr) caller)) ;(eval (fn param))
        ((eq (first expr) 'cond) (cond-eval (cdr expr) caller))
        ((eq (first expr) 'begin) (last (mapcar (lambda (e) (eval. e caller)) (cdr expr))))

        ((eq (first expr) 'get) (get1 caller (eval. (second expr) caller)))
        ((eq (first expr) 'set) (set1 caller (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'run) (run1 caller  (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'cal) (cal1 caller (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'create) (create1 caller (eval. (second expr) caller))) 
;       (t (apply (eval. (first expr) caller) (eval-list (cdr expr) caller) caller))
))

(defun get1 (caller property) ;(get color) 
  (gethash property (gethash caller *everything*)))

(defun set1 (caller property value) ;(set color red)
  (setf (gethash property (gethash caller *everything*)) value))

(defun run1 (caller func &optional params) ;(run func '(p1 p2 p3))
(setf lamb (gethash func (gethash caller *everything*)))
(setf code (sublis (pairlis (second lamb) params) (third lamb)))
(eval. code caller))

(defun cal1 (caller object expr) ;(call ball (bounce))
(run1 object 'exe (list caller expr)) ; first get the function exe from the object called
) ; caller expression

(defun create1 (caller name) ;(create ball)
(setf (gethash name *everything*) (make-hash-table))
(setf (gethash 'exe (gethash name *everything*)) 
        '(lambda (c m) ; caller expression
           (cond ((eq 'GOD c) (eval m)))))) ; replace 'GOD, with 'name



;; start
(defun repl ()
  (loop (println (eval. (read) 'GOD))))

