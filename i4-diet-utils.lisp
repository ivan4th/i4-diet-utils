(in-package :i4-diet-utils)

(defun concat (&rest values)
  "Concatenate string representations (PRINC-TO-STRING) of VALUES"
  (apply #'concatenate 'string
	 (mapcar #'princ-to-string values)))

(defun maybe-progn (body)
  "Wrap the BODY with PROGN if it contains more than one form.
Otherwise return the first form or NIL if the body is empty"
  (assert (proper-list-p body) () "body is not a proper list: ~s" body)
  (if (rest body)
      `(progn ,@body)
      (first body)))

(defun dbg (fmt &rest args)
  "Print debugging message when *prevalence-debug* is true"
  (let ((*print-readably* nil))
    (format *debug-io* "~&;; ~?~%" fmt args)))

(defmacro dbg* (title &rest args)
  (flet ((arg-ex (arg)
	   `(list ',arg ,arg)))
    `(dbg ,(concat title ": ~@{~{~s~^ = ~}~^, ~}")
	  ,@(mapcar #'arg-ex args))))

(defmacro dbg-values (form)
  (with-gensyms (result)
    `(let ((,result (multiple-value-list ,form)))
       (dbg "~s = ~{~s~^ ~}" ',form ,result)
       (apply #'values ,result))))

(defmacro dbg-show (expr)
  (with-gensyms (values)
    `(let ((,values (multiple-value-list ,expr)))
       (dbg "~s = ~{~s~^ ~}" ',expr ,values)
       (apply #'values ,values))))

(defmacro dbg-show* (&rest exprs)
  (maybe-progn
   (loop for expr in exprs collect `(dbg-show ,expr))))
