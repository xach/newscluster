;;; 
;;; template.lisp
;;; 
;;; Created: 2004-04-25 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: template.lisp,v 1.20 2008/06/18 17:48:08 xach Exp $

(in-package :newscluster)

(defvar *item-count* 30
  "The number of items to use when filling templates.")


;;; A planet template is filled up with template days

(defclass template-day ()
  ((date :initarg :date :accessor date)
   (items :initarg :items :initform nil :accessor items)))


(defun make-template-day (&key date)
  (make-instance 'template-day :date date))


(defmethod add-item ((day template-day) (item item))
  (push item (items day)))
              


;;; XML template escaping

(defun xml-escape (string)
  "Return STRING converted to something suitable for embedding as
  escaped HTML in an XML document."
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               ((#\&) (write-string "&amp;" out))
               ((#\>) (write-string "&gt;" out))
               ((#\<) (write-string "&lt;" out))
               (t
                (let ((code (char-code char)))
                  (if (> code #x7f)
                      (write-string (unicode-string code) out)
                      (write-char char out))))))))

(defun html-escape (string)
  "Return STRING converted to something suitable for embedding as
  escaped HTML in an XML document."
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               ((#\&) (write-string "&amp;" out))
               ((#\>) (write-string "&gt;" out))
               ((#\<) (write-string "&lt;" out))
               (t
                (write-char char out))))))
                                      

(defun unicode-string (char-code)
  "Convert CHAR-CODE from latin-1 (?) to a two-character string
  representing the UTF-8 coding of that character."
  (let ((string (make-array 2 :element-type 'character)))
    (setf (aref string 0)
          (code-char (logior #b11000000 (ldb (byte 2 6) char-code)))
          (aref string 1)
          (code-char (logior #b10000000 (ldb (byte 6 0) char-code))))
    string))


;;; Template file filling

(defun template-output-file (template &optional (defaults template))
  "Return a pathname based on TEMPLATE with the trailing .tmpl removed."
  (let* ((name (namestring template))
         (name-length (length name))
         (suffix ".tmpl")
         (suffix-length (length suffix)))
    (and (>= name-length suffix-length)
         (string= name suffix
                  :start1 (- name-length suffix-length))
         (let ((path (pathname (subseq name
                                       0 (- name-length suffix-length)))))
           (make-pathname :name (pathname-name path)
                          :type (pathname-type path)
                          :defaults defaults)))))


(defmethod templates ((planet planet))
  (directory (make-pathname :defaults (template-path planet)
                            :name :wild
                            :type "tmpl")))


(defmethod write-template ((planet planet) (template-file pathname)
                           (output-file pathname) values)
  (let ((*warn-on-creation* nil)
        (*template-start-marker* "<")
        (*template-end-marker* ">")
	(*string-modifier* #'identity))
    (with-open-file (out output-file
			 :external-format :latin-1
                         :direction :output
                         :if-exists :supersede)
      (fill-and-print-template template-file values :stream out))))

(defmethod write-templates ((planet planet))
  (let ((values (template-values planet))
        (*planet* planet))
    (dolist (template (templates planet))
      (let ((output-file (template-output-file template (html-path planet))))
        (ensure-directories-exist output-file)
        (write-template planet template output-file values)))))



;;; Generating values for HTML-TEMPLATE's use

(defun nonempty-string (string)
  (and (not (zerop (length string)))
       string))


(defun date/= (time1 time2)
  "Return t if TIME1 and TIME2 represent different days."
  (multiple-value-bind (second1 minute1 hour1 date1 month1 year1)
      (decode-universal-time time1 0)
    (declare (ignore second1 minute1 hour1))
    (multiple-value-bind (second2 minute2 hour2 date2 month2 year2)
        (decode-universal-time time2 0)
      (declare (ignore second2 minute2 hour2))
      (or (/= date1 date2)
          (/= month1 month2)
          (/= year1 year2)))))


(defun template-days (items)
  "Return a list of TEMPLATE-DAY objects for the items in PLANET."
  (let ((days ())
        (day nil))
    (dolist (item items (nreverse days))
      (when (or (not day)
                (date/= (date day) (date item)))
        (setf day (make-template-day :date (date item)))
        (push day days))
      (add-item day item))))


(defmethod template-values ((day template-day))
  (list :date (pretty-date (date day))
        :day (ordinal-day (date day))
        :items (nreverse (mapcar #'template-values (items day)))))


(defmethod template-values ((channel channel))
  (list :url (url channel)
        :feed-url (feed-url channel)
        :sparkline-file (enough-namestring (ensure-sparkline channel *planet*)
                                           (html-path *planet*))
        :name (name channel)
        :title (title channel)))

(defmethod template-values ((item item))
  (list :channel-url (url (channel item))
	:hexid (hexify-string (id item))
        :channel-name (name (channel item))
        :escaped-title (nonempty-string (xml-escape (title item)))
        :html-escaped-title (nonempty-string
                             (html-escape (title item)))
        :title (nonempty-string (title item))
        :pretty-title (pretty-title item)
        :link (xml-escape (link item))
        :escaped-link (xml-escape (link item))
        :description (description item)
        :escaped-description (xml-escape (description item))
        :date (pretty-date-and-time (date item))
	:iso-date (iso8601ish-timestamp (date item))
        :time (pretty-time (Date item))
	:js-timestamp (js-timestamp item)
	:time-offset (how-long-ago (date item))
        :date_rfc822 (rfc822date (date item))))

(defmethod template-values ((planet planet))
  (let ((html-items (selected-items planet 0 *item-count*)))
    (list :date (pretty-date-and-time (last-modified planet))
	  :iso-date (iso8601ish-timestamp (last-modified planet))
	  :channels (mapcar #'template-values
			    (coerce (channels planet) 'list))
	  :days (mapcar #'template-values
			(template-days html-items))
	  :planet-items (mapcar #'template-values html-items)
	  ;; XXX: Only used by the XML template
	  :archive-url (multiple-value-call #'month-url (this-month))
	  :month-sparklinks (month-sparklinks planet)
	  :items (mapcar #'template-values
			 (selected-items planet 0 10)))))


(defun month-html-file (planet year month)
  (merge-pathnames (make-pathname :directory (list :relative
                                                   (princ-to-string year))
                                  :name (princ-to-string month)
                                  :type "html")
                   (html-path planet)))

(defun month-template-file (planet)
  (merge-pathnames (make-pathname :directory '(:relative "months")
                                  :name "month.html"
                                  :type "tmpl")
                   (template-path planet)))

(defun month-url (year month)
  (format nil "~A~D/~D.html" (site-url *planet*) year month))

(defun month-link (year month)
  (format nil "<a href=\"~A\">~A</a>"
          (month-url year month)
          (pretty-month year month)))

(defun next-month-text (year month linkp)
  (multiple-value-bind (next-year next-month)
      (next-month year month)
    (if linkp
        (month-link next-year next-month)
        (pretty-month next-year next-month))))

(defun last-month-text (year month linkp)
  (multiple-value-bind (last-year last-month)
      (last-month year month)
    (if linkp
        (month-link last-year last-month)
        (pretty-month last-year last-month))))

(defmethod month-template-days ((planet planet) year month)
  (multiple-value-bind (start end)
      (month-bounds year month)
    (let ((items ()))
      (loop for channel across (channels planet) do
            (reload-channel channel)
            (load-items channel (channel-month-files channel year month))
            (loop for item across (items channel)
                     when (< start (date item) end)
                     do (push item items)))
      (template-days (sort items #'< :key #'date)))))

(defmethod write-month-template ((planet planet) year month
                                 &key (show-last-p t) (show-next-p t))
  (let ((template-file (month-template-file planet))
        (html-file (month-html-file planet year month)))
    (when (probe-file template-file)
      (let ((values (list :month-name (pretty-month year month)
                          :next-month (next-month-text year month show-next-p)
                          :last-month (last-month-text year month show-last-p)
                          :days (mapcar #'template-values
                                        (month-template-days planet
                                                             year month)))))
        (ensure-directories-exist html-file)
        (write-template planet template-file html-file values)))))

