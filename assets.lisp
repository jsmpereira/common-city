;;; -*- Mode: Lisp -*-

(in-package #:common-city)

(defparameter *sprites* (make-hash-table :test #'equal))
(defparameter *buttons* (make-hash-table :test #'equal))

(defparameter *tile-size* 16)

(defparameter *assets-dir* 
  (merge-pathnames #P"assets/" simcity-config:*base-directory*))

(defparameter *sprite-assets-hash* (make-hash-table :test #'equal))

(defun initargs-list (args)
  (let ((init-list '(:name :file :dimensions :height :tile-class :mappings)))
    (assert (= (length args) (length init-list)))
    (loop for i in init-list
	  for j in args
	  appending `(,i ,j))))

(defparameter *sprite-assets*
  `((:residential "residential.png" 9 165 complex-tile '((0 8)))
    (:commercial "commercial.png" 9 9 complex-tile '((0 8)))
    (:nuclear "nuclear.png" 16 16 complex-tile '(()))
    (:road "road.png" 1 15 sprite-tile '(()))
    (:wire "wire.png" 1 13 sprite-tile '(()))
    (:wilderness "wilderness.png" 1 38 sprite-tile '(()))
    (:garden "garden.png" 1 4 animated-tile '(()))
    (:animation-sheet "animation-sheet.png" 1 41 animated-tile '(()))
    (:industrial "industrial.png" 9 9 complex-tile '(()))
    (:fire-department "fire-department.png" 9 9 complex-tile '(()))
    (:police-department "police-department.png" 9 9 complex-tile '(()))))

(defclass asset ()
  ((name :initarg :name :accessor name)
   (file :initarg :file :accessor file)))

(defmethod initialize-instance :after ((asset asset) &key)
  (with-slots (file) asset
    (setf file (merge-pathnames file *assets-dir*))))

(defclass sprite-asset (asset)
  ((dimensions :initarg :dimensions :accessor dimensions)
   (height :initarg :height :accessor height)
   (tile-class :initarg :tile-class :accessor tile-class)
   (mappings :initarg :mappings :accessor mappings)))

(defun add-sprite (sprite-instance)
  (setf (gethash (name sprite-instance) *sprite-assets-hash*) sprite-instance))

(defun sprite-data (sprite accessor)
  "Return sprite data."
  (let ((sprite-instance (gethash sprite *sprite-assets-hash*)))
    (when sprite-instance
      (funcall accessor sprite-instance))))

(defun init-sprite (path size)
  (let* ((total-size (* size *tile-size*))
	 (sprite-sheet (sdl-image:load-image path))
	 (sprite-cells (loop for y from 0 to total-size by *tile-size*
			     append (loop for x from 0 to size by total-size
					  collect (list x y *tile-size* *tile-size*)))))
    (setf (sdl:cells sprite-sheet) sprite-cells)
    sprite-sheet))

(defun init-sprites ()
  (maphash #'(lambda (k v)
	       (setf (gethash k *sprites*) (init-sprite (file v) (height v)))) *sprite-assets-hash*))

(defun setup-sprites ()
  (dolist (s *sprite-assets*)
    (add-sprite (apply #'make-instance 'sprite-asset (initargs-list s))))
  (init-sprites))

(defparameter *road-mapping*
  '(#b1000 3
    #b0001 3
    #b1001 3
    #b1010 4
    #b0011 5
    #b0011 5
    #b0101 6
    #b1100 7
    #b1110 8
    #b1011 9
    #b0111 10
    #b1101 11
    #b1111 12))

(defparameter *button-assets*
  `(:powerplant-btn-up (,(merge-pathnames "powerplant-btn-up.png" *assets-dir*))
		       :powerplant-btn-down (,(merge-pathnames "powerplant-btn-down.png" *assets-dir*))))

(defun init-buttons ()
  (loop for indicator in *button-assets* by #'cddr do
	(multiple-value-bind (key value tail) (get-properties *button-assets* `(,indicator))
	  (setf (gethash indicator *buttons*) (sdl:load-image (first value))))))

(defparameter *audio-assets*
  `(:dozer ,(merge-pathnames "rumble.wav" *assets-dir*)
	   :bop ,(merge-pathnames "bop.wav" *assets-dir*)))
