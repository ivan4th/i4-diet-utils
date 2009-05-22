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

(defun get-real-seconds ()
  "Return INTERNAL-REAL-TIME value converted to seconds (DOUBLE-FLOAT)"
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))

(defun lispize (name)
  (with-output-to-string (out)
    (loop
       with last-caps = t
       for c across name
       when (and (not (shiftf last-caps (upper-case-p c)))
                 last-caps)
       do (princ "-" out)
       do (princ (char-upcase c) out))))

(defun capitalize (string)
  (if (zerop (length string))
      ""
      (let ((result (copy-seq string)))
        (setf (char result 0)
              (char-upcase (char result 0)))
        result)))

(defun delispize (name &optional camel-case)
  (setf name (string-downcase name))
  (labels ((rec (start)
             (let* ((pos (position #\- name :start start))
                    (sub (subseq name start pos))
                    (seg (if (and camel-case (zerop start))
                            sub
                            (capitalize sub))))
               (if pos
                   (concatenate 'string seg (rec (1+ pos)))
                   seg))))
    (rec 0)))

(defun delispize* (name &optional camel-case)
  (if (stringp name)
      name
      (delispize name camel-case)))

(defun trim (str)
  (string-trim '(#\space #\tab #\newline #\return) (or str "")))

(defun null-if-empty (str &optional trim-p)
  (let ((str (if trim-p (trim str) str)))
    (if (string= str "") nil str)))

;; FIXME!!! this 'rx' stuff is not really necessary as it's
;; already handled by CL-PPCRE compiler macros

(defvar *regex-cache* (make-hash-table :test #'equal))

(defun rx (regex &key case-insensitive-mode multi-line-mode
	   single-line-mode extended-mode destructive)
  (let ((key (list regex case-insensitive-mode multi-line-mode
		   single-line-mode extended-mode destructive)))
    (or (gethash key *regex-cache*)
	(setf (gethash key *regex-cache*)
	      (cl-ppcre:create-scanner regex :case-insensitive-mode case-insensitive-mode
				       :multi-line-mode multi-line-mode
				       :single-line-mode single-line-mode
				       :extended-mode extended-mode
				       :destructive destructive)))))

(defmacro with-match ((&rest binds) (regex target-string &rest rx-options) &body body)
  (assert (every #'symbolp binds) () "invalid binds: ~s" binds)
  (with-gensyms (matches)
    `(multiple-value-bind (whole ,matches)
	 (cl-ppcre:scan-to-strings (rx ,regex ,@rx-options) ,target-string)
       (declare (ignorable ,matches))
       (when whole
	 ,(if (null binds)
	      `(locally ,@body)
	      `(let ,(loop for n from 0
			   for name in binds
			   when name
			     collect `(,name (aref ,matches ,n)))
		 ,@body))))))

(defmacro rx-match-case (target-string &body cases)
  (with-gensyms (blk)
    (once-only (target-string)
      (labels ((expand (cases)
		 (cond ((null cases) nil)
		       ((not (and (proper-list-p (first cases))
				  (>= (length (first cases)) 2)))
			(error "RX-MATCH-CASE: invalid case: ~s" (first cases)))
		       ((eq (first (first cases)) t)
			(if (null (rest cases))
			    (rest (first cases))
			    (error "RX-MATCH-CASE: t case must be the last one")))
		       (t
			(cons
			 (destructuring-bind (rx-spec binds &rest forms) (first cases)
			   (destructuring-bind (rx &rest rx-options)
                               (ensure-list rx-spec)
			     (unless (and (stringp rx)
					  (proper-list-p binds)
					  (every #'symbolp binds)
					  (not (null forms)))
			       (error "RX-MATCH-CASE: invalid case: ~s" (first cases)))
			     `(with-match ,binds (,rx ,target-string ,@rx-options)
				(return-from ,blk ,(maybe-progn forms)))))
			 (expand (rest cases)))))))
	`(block ,blk ,(maybe-progn (expand cases)))))))