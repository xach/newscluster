;;; 
;;; planet.lisp
;;; 
;;; Created: 2004-04-11 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: planet.lisp,v 1.16 2007/04/24 19:19:14 xach Exp $

(in-package :newscluster)

(defvar *planet*)

(defclass planet ()
  ((name :initarg :name :accessor name :type string)
   (path :initform nil :accessor path) 
   (template-path :initform #p"templates/"
                  :initarg :template-path :accessor template-path)
   (html-path :initform #p"html/"
              :initarg :html-path :accessor html-path)
   (site-url :initform "http://planet.lisp.org/"
             :initarg :site-url
             :accessor site-url)
   (channels :initarg :channels :accessor channels :type vector
             :initform (make-array 25
                                   :fill-pointer 0
                                   :adjustable t))))

(defgeneric info-file-name (object))

(defgeneric info-file (object)
  (:method (object)
    (make-pathname :name (info-file-name object) :type "sexp"
                   :defaults (path object))))

(defmethod info-file-name ((planet planet))
  "planet-info")


(defmethod last-modified ((planet planet))
  (loop for channel across (channels planet)
        unless (emptyp channel)
        maximize (date (aref (items channel) 0))))


(defmethod add-channel ((planet planet) (channel channel))
  (vector-push-extend channel (channels planet)))


(defun reload-and-render (planet)
  (refresh planet)
  (write-templates planet)
  (multiple-value-bind (year month)
      (this-month)
    (write-month-template planet year month :show-next-p nil)
    (when (first-day-of-month-p)
      (multiple-value-bind (last-year last-month)
          (last-month year month)
        (write-month-template planet last-year last-month)))))


(defmethod selected-items ((planet planet) &optional (start 0) (end nil))
  (let ((items (make-array 50 :adjustable t :fill-pointer 0)))
    (loop for channel across (channels planet)
          unless (emptyp channel)
          do (loop for item across (items channel)
                   do (vector-push-extend item items)))
    (setf items (sort items #'> :key #'date))
    (coerce (if (and end
                     (> (length items) end))
                (subseq items start end)
                items)
            'list)))
                              
          

(defmethod object-storable-slots ((planet planet))
  '(name html-path site-url template-path))


(defmethod object-type ((planet planet))
  'planet)


(defun load-planet (planet-directory)
  (let ((planet (load-object (make-pathname :name "planet-info"
                                            :type "sexp"
                                            :defaults planet-directory))))
    (setf (path planet) planet-directory)
    (dolist (channel-directory (directory
                                (make-pathname :name :wild
                                               :type :wild
                                               :defaults planet-directory))
             planet)
      (when (probe-file (make-pathname :name "channel-info"
                                       :type "sexp"
                                       :defaults channel-directory))
        (add-channel planet (load-channel channel-directory))))))



(defmethod refresh ((planet planet))
  (loop for channel across (channels planet)
        when (refreshable channel)
        do (refresh channel)))


(defmethod find-channel ((planet planet) (pathname pathname))
  (loop for channel across (channels planet)
        when (equal (path channel) pathname)
        return channel))


(defmethod make-new-channel ((planet planet) directory feed-url name)
  (let* ((path (merge-pathnames directory (path planet)))
         (channel (make-instance 'channel
                                 :name name
                                 :title ""
                                 :description ""
                                 :url ""
                                 :feed-url feed-url
                                 :source "python"
                                 :current-item-files nil
                                 :path path)))
    (unless (find-channel planet path)
      (ensure-directories-exist path)
      (ensure-directories-exist (merge-pathnames #p"items/" path))
      (save-object channel (make-pathname :name "channel-info"
                                          :type "sexp"
                                          :defaults path))
      (add-channel planet channel))
    channel))


(defun archive-planet (planet)
  (let ((*planet* planet))
    (loop for channel across (channels planet)
       do (archive-channel channel))))
