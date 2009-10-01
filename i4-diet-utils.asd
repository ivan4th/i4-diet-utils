(asdf:defsystem #:i4-diet-utils
  :name "i4-diet-utils"
  :author "Ivan Shvedunov"
  :version "0.1"
  :serial t
  :description "Some utils that aren't in alexandria"
  :depends-on (:alexandria :cl-ppcre :flexi-streams :named-readtables)
  :components ((:file "package")
               (:file "i4-diet-utils")))