;;;
;;; Sparklines!!
;;;
;;; $Id: sparklines.lisp,v 1.4 2007/04/24 19:19:39 xach Exp $

(in-package :newscluster)

(defparameter *bucket-interval* (* 86400 14)
  "The timespan used to aggregate samples.")
(defparameter *bucket-count* 26
  "The number of samples that will go into a sparkline.")
(defparameter *max-sparkline-age* 86400
  "The max age, in seconds, for a sparkline file. Files are
periodically refreshed even if the user has no blog posts to make sure
the file reflects the true moribundity of the blog.")

(defun post-frequencies (channel count)
  (let ((table (make-hash-table))
        (path (path channel)))
    (flet ((process-directory (pathname)
             (dolist (file (directory pathname))
               (let ((bucket (truncate (file-write-date file)
                                       *bucket-interval*)))
                 (incf (gethash bucket table 0))))))
      (process-directory (merge-pathnames #p"archive/*/*/*.sexp" path))
      (process-directory (merge-pathnames #p"items/*.sexp" path)))
    (do ((bucket (truncate (get-universal-time) *bucket-interval*)
                 (1- bucket))
         (i 0 (1+ i))
         (result '()))
        ((= i count) result)
      (push (cons bucket (gethash bucket table 0)) result))))
        

(defun sparkline-file-name (channel planet)
  (let ((name (car (last (pathname-directory (path channel))))))
    (merge-pathnames (make-pathname :directory
                                    '(:relative "sparklines")
                                    :name name
                                    :type "gif")
                     (html-path planet))))

(defun make-sparkline (channel planet)
  (let ((spark (make-instance 'sparkline::sparkbars))
        (freqs (post-frequencies channel *bucket-count*)))
    (flet ((massage-sample (sample)
             (* sample 1)))
      (loop for ((bucket . count)) on freqs
            do (sparkline::add-sample (massage-sample count) spark))
      (sparkline::write-image spark (sparkline-file-name channel planet)))))
    
(defun ensure-sparkline (channel planet)
  "Returns a sparkline for a channel. This will create the sparkline
file if necessary, and update an existing sparkline file if it is
either older than the channel's last-modified time or if the file is
older than *MAX-SPARKLINE-AGE*."
  (let* ((file (sparkline-file-name channel planet))
         (time (and (probe-file file)
		    (file-write-date file))))
    (cond ((or (null time)
               (> (last-modified channel) time)
               (< (+ time *max-sparkline-age*) (get-universal-time)))
           (ensure-directories-exist file)
           (make-sparkline channel planet))
          (t
           file))))
          


;;; For planet archive sparklines


(defun leap-year-p (year)
  (cond ((plusp (mod year 4)) nil)
        ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t t)))

(defvar *month-lengths*
  #(31 28 31 30 31 30 31 31 30 31 30 31))

(defun days-in-month (year month)
  (let ((days (aref *month-lengths* (1- month))))
    (if (and (= month 2) (leap-year-p year))
        (1+ days)
        days)))

(defun channel-month-stats (channel year month table)
  (dolist (file (channel-month-files channel year month))
    (incf (gethash (truncate (file-write-date file) 86400) table 0)))
  (loop for item across (items channel) do
        (incf (gethash (truncate (date item) 86400) table 0))))
  

(defun month-stats (planet year month)
  (let ((samples (make-hash-table))
        (start-time (encode-universal-time 0 0 0 1 month year 0)))
    (loop for channel across (channels planet)
          do (channel-month-stats channel year month samples))
    (do ((i 0 (1+ i))
         (end (days-in-month year month))
         (day (truncate start-time 86400) (1+ day))
         (time start-time (+ start-time 86400))
         (result nil (cons (cons time (gethash day samples 0)) result)))
        ((= i end) (nreverse result)))))


(defun monthline-file-name (planet year month)
  (let ((name (format nil "~4,'0D-~2,'0D" year month)))
    (merge-pathnames (make-pathname :directory
                                    '(:relative "sparklines")
                                    :name name
                                    :type "gif")
                     (html-path planet))))

(defun month-sparkline (planet year month)
  (let ((stats (month-stats planet year month))
        (spark (make-instance 'sparkline::sparkbars :padding 1 :bar-width 1))
        (filename (monthline-file-name planet year month)))
    (loop for ((day . count)) on stats do
          (sparkline::add-sample count spark))
    (sparkline::write-image spark filename)))

(defun n-months-back (n)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (let ((result nil))
      (dotimes (i n (nreverse result))
        (push (list year month) result)
        (decf month)
        (when (zerop month)
          (setf month 12)
          (decf year))))))
  
(defun month-sparklinks (planet)
  (loop for ((year month)) on (n-months-back 12)
        for archive-url = (format nil "~A~D/~D.html" (site-url planet) year month)
        for month-sparkline = (month-sparkline planet year month)
        for sparkline-url = (format nil "~A~A"
                                    (site-url planet)
                                    (enough-namestring month-sparkline
                                                       (html-path planet)))
        collect (list :image sparkline-url
                      :label (pretty-month year month)
                      :link archive-url)))
