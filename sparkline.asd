

(defpackage :sparkline-system
  (:use :cl :asdf))

(in-package :sparkline-system)

(defsystem sparkline
  :depends-on (#:skippy)
  :components ((:file "sparkline")))
