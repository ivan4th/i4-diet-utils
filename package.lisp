(defpackage :i4-diet-utils
    (:use :cl :alexandria :flexi-streams)
  (:export #:concat
           #:maybe-progn
           #:dbg
           #:dbg-values
	   #:dbg*
	   #:dbg-show
           #:dbg-show*
           #:sv
           #:xlet
           #:xlet*
           #:xflet
           #:xlabels
           #:get-real-seconds
           #:lispize
           #:capitalize
           #:trim
           #:delispize
           #:delispize*
           #:null-if-empty
           #:with-match
           #:rx-match-case
           #:snarf-file
           #:with-input-file
           #:with-overwrite
           #:write-file
           #:unix-timestamp->universal-time
           #:universal-time->unix-timestamp
           #:format-iso8601-datetime
           #:asdf-pathname))
