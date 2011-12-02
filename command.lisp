;;; 
;;; command.lisp
;;; 
;;; Created: 2004-04-20 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: command.lisp,v 1.7 2008/06/18 17:48:08 xach Exp $

(in-package :newscluster)

(defvar *long-interval* (* 86400 30))
(defvar *short-interval* (* 86400 7))

;;;
;;; commands
;;;

(defun find-channel-by-url (planet url)
  (find url (channels planet) :key #'url :test #'string=))

(defun unknown-command (planet command args)
  (format t "; ~A: unknown command~%" command)
  (maphash (lambda (k v)
             (declare (ignore v))
             (format t "; ~A~%" k))
           *commands*))

(defun channel-match-p (string channel)
  (flet ((match (source)
           (search string source :test #'string-equal)))
    (or (match (feed-url channel))
        (match (url channel))
        (match (name channel))
        (match (title channel)))))

(defun search-channel (string planet)
  (loop for channel across (channels planet)
        when (channel-match-p string channel)
        return channel))

(defun find-channels (string planet)
  (loop for channel across (channels planet)
     when (channel-match-p string channel)
     collect channel))

(defun find-one-channel (string planet)
  (let ((channels (find-channels string planet)))
    (cond ((null channels)
           (error "No channels match ~S" string))
          ((< 1 (length channels))
           (error "Multiple channels match ~S:~%~{  ~A~%~}"
                  string
                  (mapcar #'name channels)))
          (t
           (first channels)))))

(defun update (planet command args)
  (declare (ignore command args))
  (reload-and-render planet))

(defun get-unix-time ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun modify-file-write-date (file delta)
  (let ((new-time (+ (get-unix-time) delta)))
    (zerop (sb-posix:utimes file new-time new-time))))

(defun disable-channel (planet command args)
  (declare (ignore command))
  (destructuring-bind (search-string)
      args
    (let* ((channel (find-one-channel search-string planet))
           (file (info-file channel))
           (new-name (make-pathname :type "disabled"
                                    :defaults file)))
      (rename-file (info-file channel) new-name)
      (format t "; Disabled ~A~%" (name channel)))))

(defun defer-checking (planet command args)
  (declare (ignore command))
  (flet ((checking-interval (time)
           (cond ((null time) *short-interval*)
                 ((string-equal time "long") *long-interval*)
                 ((string-equal time "short") *short-interval*)
                 (t (error "Unknown deferral timeframe ~S,~
                            try \"short\" or \"long\" instead"
                           time))))
         (interval-days (interval)
           (floor interval 86400)))
    (destructuring-bind (search-string &optional time)
        args
      (let ((channel (find-one-channel search-string planet))
            (interval (checking-interval time)))
        (format t "; Deferring checking of ~A for ~D day~:*~P~%"
                (name channel) (interval-days interval))
        (modify-file-write-date (info-file channel) interval)))))

(defun command-channel-refresh (channel planet)
  (format t "; Updating ~A from ~A~%" (name channel) (feed-url channel))
  (refresh channel)
  (write-templates planet))

(defun force-refresh (planet command args)
  (declare (ignore command))
  (destructuring-bind (search-string)
      args
    (let ((channel (find-one-channel search-string planet)))
      (modify-file-write-date (info-file channel) (- *long-interval*))
      (command-channel-refresh channel planet))))

(defun update-channel (planet command args)
  (declare (ignore command))
  (destructuring-bind (search-string)
      args
    (command-channel-refresh (find-one-channel search-string planet)
                             planet)))

(defun make-channel (planet command args)
  (declare (ignore command))
  (destructuring-bind (directory feed-url name)
      args
    (format t "; making new channel~%")
    (let ((channel (make-new-channel planet directory feed-url name)))
      (modify-file-write-date (info-file channel) (- *long-interval*))
      (command-channel-refresh channel planet))))

(defun rewrite (planet command args)
  (declare (ignore command args))
  (format t "; rewriting templates~%")
  (write-templates planet))

(defvar *commands* (make-hash-table :test 'equal))
(setf (gethash "rewrite" *commands*) 'rewrite)
(setf (gethash "update" *commands*) 'update)
(setf (gethash "update-channel" *commands*) 'update-channel)
(setf (gethash "force-refresh" *commands*) 'force-refresh)
(setf (gethash "defer-checking" *commands*) 'defer-checking)
(setf (gethash "disable-channel" *commands*) 'disable-channel)
(setf (gethash "make-channel" *commands*) 'make-channel)

(defun process-commands ()
  (flet ((error-message (control-string &rest args)
           (format t "; ~?~%" control-string args)))
    (if (>= (length sb-ext:*posix-argv*) 3)
        (destructuring-bind (planet-directory command &rest args)
            (cdr sb-ext:*posix-argv*)
          (let ((func (gethash command *commands*))
                (planet-dir (probe-file planet-directory)))
            (cond ((and func planet-dir)
                   (let ((*planet* (load-planet planet-dir)))
                     (handler-case
                         (funcall func *planet* command args)
                       (simple-error (condition)
                         (format t "An error occurred: ")
                         (write condition :escape nil :readably nil)
                         (return-from process-commands)))))
                  ((not planet-dir)
                   (error-message "directory not found: ~A" planet-directory))
                  ((not func)
                   (error-message "unknown command: ~A" command)
                   (maphash (lambda (k v)
                              (declare (ignore v))
                              (format t "; ~A~%" k))
                            *commands*))
                  (t (error-message "totally fucked up")))))
        (error-message "insufficient command line args: ~A ~A" (length sb-ext:*posix-argv*) sb-ext:*posix-argv*)))
  (sb-ext:quit))

