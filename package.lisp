(defpackage :i4-diet-utils
  (:use :cl :alexandria)
  (:export #:concat
           #:maybe-progn
           #:dbg
           #:dbg-values
	   #:dbg*
	   #:dbg-show
           #:dbg-show*
           #:get-real-seconds
           #:lispize
           #:capitalize
           #:trim
           #:delispize
           #:delispize*
           #:null-if-empty
           #:with-match
           #:rx-match-case))
