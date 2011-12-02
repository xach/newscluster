;; sparkline.lisp

(defpackage :sparkline
  (:use :cl))

(in-package :sparkline)

(defclass sparkline ()
  ((samples
    :initarg :samples
    :reader samples))
  (:default-initargs :samples (make-array 10
                               :adjustable t
                               :fill-pointer 0
                               :initial-element 0)))

(defgeneric add-sample (sample sparkline)
  (:method (sample sparkline)
    (vector-push-extend sample (samples sparkline))))

(defgeneric sample-count (sparkline)
  (:method (sparkline)
    (length (samples sparkline))))

(defgeneric clear-samples (sparkline)
  (:method (sparkline)
    (setf (fill-pointer (samples sparkline)) 0)))

(defgeneric graphic-width (sparkline))

(defgeneric graphic-height (sparkline))

(defgeneric make-graphic-canvas (sparkline))

(defgeneric render-sample (n sparkline canvas))

(defgeneric save-canvas (canvas pathname))

(defgeneric write-image (sparkline pathname))

(defmethod write-image (sparkline pathname)
  (let ((canvas (make-graphic-canvas sparkline)))
    (dotimes (i (sample-count sparkline))
      (render-sample i sparkline canvas))
    (save-canvas canvas pathname)))



(defclass canvas ()
  ((height
    :initarg :height
    :initform (error "~A required" :height)
    :reader height)
   (width
    :initarg :width
    :initform (error "~A required" :width)
    :reader width)
   (data
    :reader data
    :writer (setf %data))))

(defgeneric clear-canvas (canvas))



(defparameter *basic-palette*
  (skippy:make-color-table :initial-contents 
                           '(#x000000
                             #xFFFFFF
                             #xFF0000
                             #x00FF00
                             #x0000FF
                             #x666666
                             #x999999
                             #xCCCCCC)))

(defconstant +black+  0)
(defconstant +white+  1)
(defconstant +red+    2)
(defconstant +green+  3)
(defconstant +blue+   4)
(defconstant +gray0+  5)
(defconstant +gray1+  6)
(defconstant +gray2+  7)

(defclass gif-canvas (canvas)
  ())

(defmethod initialize-instance :after ((instance gif-canvas) &key height width)
  (setf (%data instance)
        (make-array (* width height)
                    :element-type '(unsigned-byte 8)
                    :initial-element +white+)))

(defun draw-gif-bar (x y width height index canvas)
  (let ((cw (width canvas))
        (data (data canvas))
        (y (- (- (height canvas) y) height)))
    (do* ((start (+ x (* cw y)) (+ start cw))
          (end (+ start width) (+ start width))
          (i 0 (1+ i)))
         ((= i height))
      (fill data index :start start :end end))))

(defmethod clear-canvas ((canvas gif-canvas))
  (fill (data canvas) +white+)
  (values))

(defmethod save-canvas ((canvas gif-canvas) pathname)
  (let* ((ds (skippy:make-data-stream :height (height canvas)
                                      :width (width canvas)))
         (image (skippy:make-image :height (height canvas)
                                   :width (width canvas)
                                   :image-data (data canvas)
                                   :color-table *basic-palette*
                                   :transparency-index 1)))
    (skippy:add-image image ds)
    (skippy:output-data-stream ds pathname)))




(defclass sparkbars (sparkline)
  ((max-height
    :initarg :max-height
    :accessor max-height)
   (padding
    :initarg :padding
    :accessor padding)
   (bar-width
    :initarg :bar-width
    :accessor bar-width))
  (:default-initargs
   :max-height 16
   :padding 1
   :bar-width 2))
   
(defmethod make-graphic-canvas ((sparkbars sparkbars))
  (make-instance 'gif-canvas
                 :width (graphic-width sparkbars)
                 :height (graphic-height sparkbars)))

(defmethod graphic-width ((sparkbars sparkbars))
  (let ((padding (padding sparkbars))
        (bar-width (bar-width sparkbars)))
    (+ padding (* (sample-count sparkbars) (+ bar-width padding)))))

(defmethod graphic-height ((sparkbars sparkbars))
  (max-height sparkbars))

(defmethod render-sample (n (sparkline sparkbars) (canvas gif-canvas))
  (let ((sample (aref (samples sparkline) n))
        (max-sample-height (1- (max-height sparkline)))
        (width (bar-width sparkline))
        (offset (+ 1 (* (+ (bar-width sparkline) (padding sparkline)) n))))
    (cond ((zerop sample)
           (draw-gif-bar offset 0
                         width 1
                         +gray2+ canvas))
          (t
           (draw-gif-bar offset 0
                         width
                         (min sample max-sample-height)
                         +gray1+ canvas)
           (when (> sample max-sample-height)
             (draw-gif-bar offset max-sample-height
                           width 1
                           +red+ canvas))))))
