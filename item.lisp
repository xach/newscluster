;;; 
;;; item.lisp
;;; 
;;; Created: 2004-04-11 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: item.lisp,v 1.4 2008/06/18 17:48:08 xach Exp $


(in-package :newscluster)


(defclass item ()
  ((id :initarg :id :accessor id)
   (title :initarg :title :accessor title :type string)
   (description :initarg :description :accessor description :type string)
   (author-name :initarg :author-name :accessor author-name :type string)
   (date :initarg :date :accessor date :type integer)
   (link :initarg :link :accessor link :type string)
   (channel :initform nil :accessor channel))
  (:default-initargs
   :author-name ""))

(defmethod print-object ((item item) stream)
  (print-unreadable-object (item stream :type t)
    (format stream "~S ~S" (id item) (title item))))


(defun make-item (id title author-name description date link)
  (make-instance 'item
                 :id id
                 :title title
                 :author-name author-name
                 :description description
                 :date date
                 :link link))


(defmethod object-type ((item item))
  'item)

(defmethod object-storable-slots ((item item))
  '(id title author-name description date link))

(defun load-item (source)
  (load-object source))


(defun hexify-string (string)
  (with-output-to-string (out)
    (loop for char across string
          do (format out "~16,2,'0,,R" (char-code char)))))


(defmethod pretty-title ((item item))
  (format nil "~A~@[ - ~A~]"
          (name (channel item))
          (nonempty-string (title item))))


(defun planet-archive-path (planet)
  (merge-pathnames (make-pathname :directory '(:relative :up "archive"))
                   (path planet)))

(defun item-relative-path (item-file)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (file-write-date item-file) 0)
    (declare (ignore second minute hour day))
    (let ((year-string (princ-to-string year))
          (month-string (princ-to-string month)))
      (make-pathname :directory (list :relative :up
                                      "archive" year-string month-string)
                     :defaults item-file))))


(defun item-archive-path (item-file)
  (merge-pathnames (item-relative-path item-file) item-file))
                                                

(defun archive-item-file (item-file)
  (let ((archive-file (item-archive-path item-file)))
    (ensure-directories-exist archive-file)
    (rename-file item-file archive-file)))

(defun js-timestamp (item)
  (let ((offset (encode-universal-time 0 0 0 1 1 1970 0)))
    (* 1000
       (- (date item) offset))))
