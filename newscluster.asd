;;; 
;;; newscluster.asd
;;; 
;;; Created: 2004-04-17 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: newscluster.asd,v 1.6 2007/04/24 18:20:30 xach Exp $

(defpackage :newscluster-system
  (:use :cl :asdf))

(in-package :newscluster-system)


(defsystem #:newscluster
  :depends-on (:HTML-TEMPLATE :sparkline #:sb-posix)
  :components ((:file "package")
               (:file "persistence" :depends-on ("package"))
               (:file "fetch" :depends-on ("package"))
               (:file "dates" :depends-on ("package"))
               (:file "item" :depends-on ("persistence"))
               (:file "channel" :depends-on ("item"))
               (:file "planet" :depends-on ("channel"
                                            "item" "dates"
                                            "fetch"))
               (:file "sparklines" :depends-on ("planet"
                                                "channel"))
               (:file "template" :depends-on ("planet"
                                              "sparklines"))
               (:file "command" :depends-on ("planet"))))
