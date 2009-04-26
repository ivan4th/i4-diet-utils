(defpackage :i4-diet-utils
  (:use :cl :alexandria)
  (:export #:concat
           #:maybe-progn
           #:dbg
           #:dbg-values
	   #:dbg*
	   #:dbg-show
           #:dbg-show*
           #:trim
           #:get-real-seconds))
