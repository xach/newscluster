;;; 
;;; dates.lisp
;;; 
;;; Created: 2004-04-15 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: dates.lisp,v 1.8 2005/10/31 15:23:45 xach Exp $

(in-package :newscluster)

(defvar *day-of-week-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar *short-month-names*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *full-month-names*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defparameter *ordinal-suffixes*
  #("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th" "th" "th" "th" "th"))

(defun ordinal-day (time)
  (let ((day (nth-value 3 (decode-universal-time time 0))))
    (format nil "~D~A" day (aref *ordinal-suffixes*
                                 (or (and (< day 14) day)
                                     (mod day 10))))))


(defun rfc822date (time)
  (multiple-value-bind (second minute hour date month year
                        day-of-week daylight-p tz)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0D ~A ~
                 ~D ~2,'0D:~2,'0D:~2,'0D GMT"                         
            (nth day-of-week *day-of-week-names*) date
            (nth (1- month) *short-month-names*) year                       
            hour minute second)))

(defun pretty-date (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (declare (ignore second minute hour))
    (format nil "~A ~D, ~D" (nth (1- month) *full-month-names*) date year)))

(defun iso8601-timestamp (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0DZ"
	    year month date hour minute)))

(defun iso8601ish-timestamp (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
	    year month date hour minute)))

(defun pretty-month (year month)
  (format nil "~A, ~D" (elt *full-month-names* (1- month)) year))

(defun pretty-date-and-time (time)
  (multiple-value-bind (second minute hour)
      (decode-universal-time time 0)
    (declare (ignore second))
    (flet ((12-hour (hour)
             (if (or (zerop hour)
                     (= 12 hour))
                 12
                 (mod hour 12))))
      (format nil "~A ~2,'0D:~2,'0D ~A"
              (pretty-date time)
              (12-hour hour) minute
              (if (< hour 12) "AM" "PM")))))


(defun pretty-time (time)
  (multiple-value-bind (second minute hour)
      (decode-universal-time time 0)
    (declare (ignore second))
    (flet ((12-hour (hour)
             (if (or (zerop hour)
                     (= 12 hour))
                 12
                 (mod hour 12))))
      (format nil "~2,'0D:~2,'0D ~A"
              (12-hour hour) minute
              (if (< hour 12) "AM" "PM")))))


(defun next-month (year month)
  "Return the YEAR and MONTH of the next month, as multiple values."
  (if (= month 12)
      (values (1+ year) 1)
      (values year (1+ month))))

(defun last-month (year month)
  "Return the YEAR and MONTH of the last month, as multiple values."
  (if (= month 1)
      (values (1- year) 12)
      (values year (1- month))))
    

(defun this-month ()
  "Return the current YEAR and MONTH as multiple values."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour day))
    (values year month)))


(defun first-day-of-month-p ()
  "Return T if today is the first day of the month."
  (= 1 (nth-value 3 (decode-universal-time (get-universal-time) 0))))

(defun month-bounds (year month)
  "Return the start and end of the given MONTH as two universal
times."
  (let* ((next-month (if (< month 12)
                         (1+ month)
                         1))
         (next-year (if (= month 12) (1+ year) year)))
    (values (encode-universal-time 0 0 0 1 month year 0)
            (encode-universal-time 0 0 0 1 next-month next-year 0))))

(defun how-long-ago (time)
  (let* ((diff (- (get-universal-time) time))
	 (minute 60)
	 (hour (* minute 60))
	 (day (* hour 24)))
    (cond ((minusp diff) "0 minutes ago")
	  ((< diff (* minute 60))
	   (format nil "~D minute~:P ago" (floor diff minute)))
	  ((< diff (* hour 48))
	   (format nil "~D hour~:P ago" (floor diff hour)))
	  (t
	   (format nil "~D day~:P ago" (floor diff day))))))
