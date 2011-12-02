;;; 
;;; fetch.lisp
;;; 
;;; Created: 2004-04-18 by Zach Beane <xach@xach.com>
;;; 
;;; **PURPOSE**
;;; 
;;; 
;;; $Id: fetch.lisp,v 1.5 2004/07/20 21:16:45 xach Exp $

(in-package :newscluster)

(defvar *python-fetch-program*
  (asdf:system-relative-pathname "newscluster" #p"python/fetch-channel.py"))

(defun make-fetcher (channel)
  (cond ((string= (source channel) "python")
         (lambda ()
           (unless (simple-run-program (native-filename *python-fetch-program*)
                                       (feed-url channel)
                                       (native-filename (path channel))
                                       (name channel))
             (format *debug-io* "; fetch of ~A failed!~%"
                     (feed-url channel)))))))


(defun native-filename (pathname)
  (let ((directory (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (with-output-to-string (s nil :element-type 'base-char)
      (etypecase directory
        (string (write-string directory s))
        (list
         (when (eq (car directory) :absolute)
           (write-char #\/ s))
         (dolist (piece (cdr directory))
           (etypecase piece
             (string (write-string piece s) (write-char #\/ s))))))
      (etypecase name
        (null)
        (string (write-string name s)))
      (etypecase type
        (null)
        (string (write-char #\. s) (write-string type s))))))


(defun simple-run-program (program &rest args)
  "Return T if PROGRAM runs and exits with status 0."
  (let* ((stringified-args
          (mapcar #'(lambda (obj)
                      (if (stringp obj)
                          obj
                          (format nil "~A" obj)))
                  args))
         (process (sb-ext:run-program program stringified-args :output t)))
    (and process
         (zerop (sb-ext:process-exit-code process)))))


