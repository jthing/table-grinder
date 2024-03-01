;;; utils.lisp -- Rapid prototyping utillities
(defpackage #:utils
  (:use #:cl #+sbcl #:sb-ext)
  (:shadow :!)
  (:local-nicknames (:locks :cl-package-locks))
  (:export :! :@ :ø :alias :help :while
   :it :if-the :when-the :while-the :if-success-the :while-success-the :do-file :compose :foldl
   :collect-if :range :make-flexi-string
   :freeze-flexi-string :stringify
   :the-last :unary-p :push-end :list! :npush-end :curry :flatten :partition
   :partition-string :enter-playpen :leave-playpen :compose :op-fn :op-if
   :op-and :op-or :foldl :tree-traverse :dbind :vbind :vcall :vsetq #:zip #:duplicate #:intersperse
   #:string-join #:permiate-strings #:path->list #:list! #:strcat #:cleanup-path #:append-path :primep))

(in-package #:utils)

(defun ! (string) (progn (inferior-shell:run string)) (values))

(defmacro @ (&body body) `(ignore-errors ,@body))

(defmacro ø (&body body) `(progn ,@body (values)))

(defun help (command)
  (progn
    (princ
     (typecase command
       (function (documentation command 'function))
       (symbol (documentation command 'variable))))
    (values)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro if-the (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro when-the (test-form &body body)
  `(if-the ,test-form
	   (progn ,@body)))

(defmacro while-the (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro if-success-the (test &optional then else)
  "if-success-the -- Pre: 'test' MUST return fail as second value on stack
If 'test' succeeds set it in the variable 'it' for use in the 'body'."
  (let ((ok (gensym)))
    `(multiple-value-bind (it ,ok) ,test
       (if (or ,ok it) ,then ,else))))

(defmacro when-sucess-the (test &body body)
  "when-success-the-- Pre: 'test' MUST return fail as second value on stack
If 'test' succeeds set the variable 'it' for use in the 'body'."
  `(aif2 ,test
	 (progn ,@body)))

(defmacro while-success-the (test &body body)
  "while-success-the -- Pre: 'test' MUST return fail as second value on stack
If 'test' succeeds set the variable 'it' for use in the body."
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	      (if-success-the ,test
			      (progn ,@body)
			      (setq ,flag nil))))))

(let ((fail (gensym "read-fail")))
  (defun read-successfully (&optional (str *standard-input*))
    (let ((val (read str nil fail)))
      (unless (equal val fail) (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (while-success-the (read-successfully ,str)
			  ,@body))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defmacro op-fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

(defun op-if (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun op-and (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'op-and fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun op-or (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'op-or fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

(defun foldl (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

(defun tree-traverse (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  (flet ((pair (list)
	   (loop for (_1 _2) on list by #'cddr collect (list _1 _2))))
    `(progn
       ,@(mapcar #'(lambda (pair)
		     `(abbrev ,@pair))
		 (pair names)))))

(defmacro alias (short long)
  (if (eql (type-of long) 'compiled-function)
      `(setf (fdefinition (quote ,short)) (fdefinition (quote ,long)))
      `(setf (fdefinition (quote ,short)) (compile nil ,long))))

(abbrevs dbind destructuring-bind
         vbind multiple-value-bind
         vcall multiple-value-call
         vsetq multiple-value-setq)

(defun collect-if (test-p list)
  "collect-if -- return list of elements in 'list' that match 'test-p'"
  (let (new-list)
    (dolist (element list)
      (when (funcall test-p element)
        (push element new-list)))
    (nreverse new-list)))

(defun range (&rest list)
  "range -- make a list of elements
(range 5)     - list 0 .. 5(help)
(range 1 5)   - list 1 .. 5
(range 1 5 2) - list 1,3,5
"
  (let ((min 0) max (step 1))
    (cond
      ((= (length list) 1) (setf max (first list)))
      ((= (length list) 2) (setf min (first list)) (setf max (second list)))
      ((= (length list) 3) (setf min (first list)) (setf max (second list)) (setf step (third list)))
      (t (error "Wrong number of arguments")))
    (loop for n from min to max by step collect n)))

(defun make-flexi-string (&optional string)
  (if string
      (make-array (length string) :element-type 'base-char :initial-contents string
				  :fill-pointer (1- (length string)) :adjustable t)
      (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t)))

(defun freeze-flexi-string (string)
  (make-array (length string) :element-type 'base-char :initial-contents string))

(defun stringify (&rest source)
  (let ((sink (make-flexi-string)))
    (with-output-to-string (stream sink)
      (dolist (element source)
        (princ element stream)))
    sink))

(proclaim '(inline the-last unary-p push-end npush-end list!))

(defun the-last (list)
  (car (last list)))

(defun unary-p (list)
  (and (consp list) (not (cdr list))))

(defun push-end (list object)
  (append list (list object)))

(defun npush-end (list object)
  (nconc list (list object)))

(defun curry (fun &rest args)
  (lambda (&rest more) (apply fun (append args more))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun partition (source n)
  (let* ((length (length source))
	 (end (- length (rem length n)))
	 sink)
    (do
     ((i 0 (1+ i)))
     ((= i end))
      (if (= (rem i n) 0)
	  (push (subseq source i (+ i n)) sink)))
    (unless (= end length)
      (push (subseq source end length) sink))
    (nreverse sink)))

(defun zip (list-1 list-2)
  (flatten (mapcar #'list list-1 list-2)))

(defun duplicate (item times)
  (let (result)
    (dotimes (_i times)
      (push item result))
    result))

(defun intersperse (list sep)
  (loop :for element :on list
	:collect (car element)
	:if (cdr element)
	  :collect sep))

(defun shuffle (sequence)
  (loop with target = (copy-seq sequence)
	with length = (length target)
	for i from 0 to (1- length) do
	  (rotatef (elt target i) (elt target (+ i (random (- length i)))))
	finally (return target)))

(alias strcat (curry #'concatenate 'string))

(defun partition-string-char (string)
  (map 'list #'identity string))

(defun string-join (sep &rest strings)
  "Reduce a list of strings to a single string, inserting a separator between them."
   (apply #'strcat (intersperse strings sep)))

(defun permiate-strings (lst sep)
  (strcat sep (string-join lst sep) sep))

(defun path->list (path)
  (rest (pathname-directory path)))

(declaim (ftype (function (fixnum) T) primep))
(defun primep (n)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum n))
  (cond
    ((= n 1) nil)
    ((= n 2) t)
    ((zerop (mod n 2)) nil)
    (t
     (loop for i of-type fixnum from 3 to (1+ (isqrt n)) by 2 do
       (when (zerop (the fixnum (mod n i))) (return-from primep nil)))
     (return-from primep t))))

(defun append-path (path1 path2)
  (permiate-strings (append (path->list path1) (path->list path2)) "/"))

(defun cleanup-path ()
  "My environmemt variable PATH has duplicate entries.. Lets fix that!"
  (let* ((path-list (uiop:split-string (uiop:getenv "PATH") :separator ":")) ; fist split the line at : to n paths
	 (cleaned-path-list (remove-duplicates (sort path-list #'string<=))) ; fine, now sort and remove duplicates
         ;(path-string (apply #'strcat (intersperse cleaned-path-list ":")))) ; put it back in string form adding the :'s back
	 (path-string (format nil "~{~A~^:~}" cleaned-path-list)))
    (setf (uiop:getenv "PATH") path-string)))

(let (pre-playpen-package playpen-package)

  (defun enter-playpen ()
    (setf pre-playpen-package *package*)
    (locks:without-package-locks
      (setf playpen-package
	    (make-package :playpen-package
			  :use '(#:cl #+sbcl #:sb-alien #+sbcl #:sb-debug #+sbcl #:sb-ext
				 #+sbcl  #:sb-gray #+sbcl #:sb-profile #:utils)))
      (setf *package* playpen-package)))

  (defun leave-playpen ()
    (locks:without-package-locks
      (setf *package* pre-playpen-package))
    (delete-package playpen-package)))
