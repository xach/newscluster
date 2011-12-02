;;; 
;;; channel.lisp
;;; 
;;; Created: 2004-04-11 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: channel.lisp,v 1.18 2007/04/24 19:18:07 xach Exp $

(in-package :newscluster)

(defvar *item-expire-days* 14)

(defvar *loaded-channels* (make-hash-table :test 'equal))

(defvar *check-interval* 3600
  "The time, in seconds, to wait between fetches of channels.")

(defclass channel ()
  ((name :initarg :name :accessor name :type string)
   (title :initarg :title :accessor title :type string)
   (description :initarg :description :accessor description :type string)
   (items :initarg :items :accessor items :type vector
          :initform (make-array 25
                                :adjustable t
                                :fill-pointer 0
                                :element-type 'item))
   (url :initarg :url :accessor url :type string)
   (path :initarg :path :accessor path)
   (feed-url :initarg :feed-url :accessor feed-url :type string)
   (source :initarg :source :accessor source)
   (current-item-files :initarg :current-item-files
                       :initform nil
                       :accessor current-item-files)
   (check-frequency :initarg :check-frequency :initform 3600
                    :accessor check-frequency)
   (last-fetch-time :initarg :last-fetch-time
                    :initform 0
                    :accessor last-fetch-time)))


(defmethod object-storable-slots ((channel channel))
  '(name title description url feed-url source check-frequency
    current-item-files))


(defmethod object-type ((channel channel))
  'channel)

(defmethod info-file-name ((channel channel))
  "channel-info")

(defun expiredp (item-file)
  (< (+ (file-write-date item-file)
        (* 86400 *item-expire-days*))
     (get-universal-time)))

(defun failure-count (channel)
  (let ((pathname (merge-pathnames "failure-count" (path channel))))
    (if (probe-file pathname)
	(with-open-file (stream pathname)
          (parse-integer (read-line stream)))
        0)))

(defun refresh-interval (channel)
  (+ *check-interval*
     (* 3600 (expt (failure-count channel) 2))))

(defun refreshable (channel)
  (< (+ (last-fetch-time channel)
	(refresh-interval channel))
     (get-universal-time)))

(defun channel-item-files (channel)
  (let ((wild (make-pathname :directory '(:relative "items")
                             :name :wild
                             :type "sexp")))
    (directory (merge-pathnames wild (path channel)))))

(defun channel-feed-files (channel)
  (loop with channel-path = (path channel)
        for file in (current-item-files channel)
        for full-path = (make-pathname :directory '(:relative "items")
                                       :defaults file)
        when (probe-file (merge-pathnames full-path channel-path))
        collect it))

(defun channel-active-files (channel)
  (union (channel-feed-files channel)
         (loop for file in (channel-item-files channel)
               unless (expiredp file)
               collect file)
         :test #'equal))

(defun channel-inactive-files (channel)
  (set-difference (channel-item-files channel)
                  (channel-active-files channel)
                  :test #'equal))

(defmethod last-modified ((channel channel))
  (cond ((plusp (length (items channel)))
         (let ((first-item (aref (items channel) 0)))
           (date first-item)))
        (t 0)))

(defun load-channel (channel-directory)
  (let* ((channel-info-file (make-pathname :name "channel-info"
                                           :type "sexp"
                                           :defaults channel-directory))
         (channel (load-object channel-info-file)))
    (setf (path channel) channel-directory
          (items channel) (make-array 25 :adjustable t :fill-pointer 0))
    (setf (gethash channel-directory *loaded-channels*) channel)
    (setf (last-fetch-time channel) (file-write-date channel-info-file))
    (load-items channel (channel-active-files channel))
    channel))


(defun load-items (channel item-files)
  (dolist (item (mapcar #'load-item item-files) (sort-items channel))
    (setf (channel item) channel)
    ;; FIXME: Almost anything would be more efficient to ensure
    ;; that two items with the same link are not added to a channel
    (unless (find (id item) (items channel) :key #'id :test #'string=)
      (vector-push-extend item (items channel)))))
  


(defun reload-channel (channel)
  (let ((new-channel (load-channel (path channel))))
    (setf (title channel) (title new-channel)
          (description channel) (description new-channel)
          (items channel) (items new-channel)
          (current-item-files channel) (current-item-files new-channel)
          (last-fetch-time channel) (last-fetch-time new-channel)
          (url channel) (url new-channel))))


(defmethod sort-items ((channel channel))
  (setf (items channel) (sort (items channel) #'> :key #'date)))


(defmethod add-item ((channel channel) (item item))
  (setf (channel item) channel)
  (vector-push-extend item (items channel))
  (sort-items channel))


(defmethod emptyp ((channel channel))
  (zerop (length (items channel))))
  

(defmethod refresh ((channel channel))
  (let ((fetcher (make-fetcher channel)))
    (when fetcher
      (funcall fetcher)
      (reload-channel channel)
      (archive-channel channel))))


(defun archive-channel (channel)
  (dolist (file (channel-inactive-files channel))
    (archive-item-file file)))

(defun channel-month-files (channel year month)
  (let ((year-string (prin1-to-string year))
        (month-string (prin1-to-string month)))
    (directory
     (merge-pathnames (make-pathname :directory (list :relative
                                                      "archive"
                                                      year-string month-string)
                                     :name :wild
                                     :type "sexp")
                      (path channel)))))
