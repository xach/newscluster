;;; 
;;; persistence.lisp
;;; 
;;; Created: 2004-04-11 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: persistence.lisp,v 1.3 2007/04/24 19:18:49 xach Exp $

(in-package :newscluster)

(defgeneric object-storable-slots (object)
  (:documentation "Return a list of slots that can be saved for for objects."))


(defgeneric object-type (object)
  (:documentation
   "Return the storable object type for OBJECT."))


(defmethod save-object (object (stream stream))
  (flet ((keywordify (symbol)
           (intern (symbol-name symbol) (find-package "KEYWORD"))))
    (let ((form (loop for slot-name in (object-storable-slots object)
                      collect (keywordify slot-name)
                      collect (slot-value object slot-name)))
          (*print-pretty* nil))
      (print (cons (object-type object) form) stream))))


(defmethod save-object (object (pathname pathname))
  (with-open-file (stream pathname :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (save-object object stream)))


(defmethod object-initargs (object-type args)
  args)


(defmethod load-object ((stream stream))
  (let ((*read-eval* nil))
    (multiple-value-bind (form error)
        (ignore-errors (read stream nil))
      (when error
        (warn error))
      (when form
        (let ((type (car form)))
          (apply #'make-instance
                 type
                 (object-initargs type (cdr form))))))))


(defmethod load-object ((pathname pathname))
  (with-open-file (stream pathname :direction :input
                          :external-format :latin-1)
    (load-object stream)))


(defmethod load-object ((string string))
  (with-input-from-string (stream string)
    (load-object stream)))
