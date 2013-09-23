;;; -*- Mode: Lisp -*-

(in-package #:common-city)

(defparameter *tile-size* 16)

(defparameter *assets-dir* 
  (merge-pathnames #P"assets/" simcity-config:*base-directory*))

(defparameter *sprite-assets* (make-hash-table :test #'equal))
(defparameter *button-assets* (make-hash-table :test #'equal))

(defparameter *sprite-specs*
  `((:residential "residential.png" 9 9 complex-tile '((0 8)))
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

(defparameter *button-specs*
  `((:dozer "dozer-btn.png" 34 2 button-tile '() "Bulldoze" 1)
    (:residential "residential-btn.png" 50 2 button-tile '() "Residential" 100)
    (:nuclear "nuclear-btn.png" 42 2 button-tile '(()) "Nuclear Plant" 5000)))

(defclass asset ()
  ((surface :initarg :surface :accessor surface)
   (name :initarg :name :accessor name)
   (path :initarg :path :accessor path)))

(defmethod initialize-instance :after ((asset asset) &key)
  (with-slots (path) asset
    (setf path (merge-pathnames path *assets-dir*))))

(defgeneric add-asset (asset)
  (:documentation "Adds the given asset to the appropriate collection."))

(defclass sprite-asset (asset)
  ((dimensions :initarg :dimensions :accessor dimensions)
   (height :initarg :height :accessor height)
   (tile-class :initarg :tile-class :accessor tile-class)
   (mappings :initarg :mappings :accessor mappings)))

(defmethod initialize-instance :after ((sprite-asset sprite-asset) &key)
  (with-slots (path height dimensions surface) sprite-asset
    (let* ((tile-size (max dimensions *tile-size*))
	   (total-size (* height tile-size))
	   (sprite-sheet (sdl-image:load-image path))
	   (sprite-cells (loop for y from 0 below total-size by tile-size
			       append (loop for x from 0 below height by total-size
					    collect (list x y tile-size tile-size)))))
      (setf (sdl:cells sprite-sheet) sprite-cells)
      (setf surface sprite-sheet))))

(defmethod add-asset ((sprite-asset sprite-asset))
  (setf (gethash (name sprite-asset) *sprite-assets*) sprite-asset))

(defclass button-asset (sprite-asset)
  ((tooltip :initarg :tooltip :accessor tooltip)
   (cost :initarg :cost :accessor cost)))

(defmethod add-asset ((button-asset button-asset))
  (setf (gethash (name button-asset) *button-assets*) button-asset))

(defun draw-tooltip (tooltip cost)
  (sdl:draw-string-solid-* (format nil "~A: $~A" tooltip cost)  10 10 :surface (surface *text-surface*)))

(defun asset-data (asset-key accessor &key (collection *sprite-assets*))
  "Return asset data."
  (let ((asset-instance (gethash asset-key collection)))
    (when asset-instance
      (funcall accessor asset-instance))))

(defun initargs-list (asset-spec class)
  (closer-mop:finalize-inheritance (find-class class))
  (let ((arg-list (mapcan #'closer-mop:slot-definition-initargs
			  (closer-mop:class-slots (find-class class)))))
    (loop for i in (rest arg-list)
	  for j in asset-spec
	  appending `(,i ,j))))

(defun init-assets (collection class)
  (dolist (asset-spec collection)
    (add-asset (apply #'make-instance class (initargs-list asset-spec class)))))

(defun setup-assets ()
  (init-assets *sprite-specs* 'sprite-asset)
  (init-assets *button-specs* 'button-asset))

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

(defparameter *audio-assets*
  `(:dozer ,(merge-pathnames "rumble.wav" *assets-dir*)
	   :bop ,(merge-pathnames "bop.wav" *assets-dir*)))
