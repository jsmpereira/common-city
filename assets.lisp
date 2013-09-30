;;; -*- Mode: Lisp -*-

(in-package #:common-city)

(defparameter *tile-size* 16)

(defparameter *assets-dir* 
  (merge-pathnames #P"assets/" simcity-config:*base-directory*))

(defparameter *sprite-assets* (make-hash-table :test #'equal))
(defparameter *button-assets* (make-hash-table :test #'equal))

(defparameter *sprite-specs*
  `((:residential "residential.png" 9 complex-tile '((0 8)))
    (:commercial "commercial.png" 9 complex-tile '((0 8)))
    (:nuclear "nuclear.png" 16 complex-tile '(()))
    (:road "road.png" 1 sprite-tile '(()))
    (:rail "rail.png" 1 sprite-tile '(()))
    (:wire "wire.png" 1 sprite-tile '(()))
    (:wilderness "wilderness.png" 1 sprite-tile '(()))
    (:garden "garden.png" 1 animated-tile '(()))
    (:animation-sheet "animation-sheet.png" 1 animated-tile '(()))
    (:industrial "industrial.png" 9 complex-tile '(()))
    (:fire-department "fire-department.png" 9 complex-tile '(()))
    (:police-department "police-department.png" 9 complex-tile '(()))))

(defparameter *button-specs*
  `((:dozer "dozer-btn.png" 2 button-tile '() "Bulldoze (D)" 1)
    (:road "road-btn.png" 2 button-tile '() "Road (R)" 10)
    (:rail "rail-btn.png" 2 button-tile '() "Rail (T)" 20)
    (:wire "wire-btn.png" 2 button-tile '() "Power Lines (W)" 5)
    (:garden "garden-btn.png" 2 button-tile '() "Park (G)" 10)
    (:residential "residential-btn.png" 2 button-tile '() "Residential (H)" 100)
    (:commercial "commercial-btn.png" 2 button-tile '() "Commercial (C)" 100)
    (:industrial "industrial-btn.png" 2 button-tile '() "Industrial (I)" 100)
    (:police-department "police-department-btn.png" 2 button-tile '() "Police Department (P)" 500)
    (:fire-department "fire-department-btn.png" 2 button-tile '() "Fire Department (F)" 500)
    (:nuclear "nuclear-btn.png" 2 button-tile '(()) "Nuclear Plant (N)" 5000)))

(defclass asset ()
  ((surface :initarg :surface :accessor surface)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (name :initarg :name :accessor name)
   (path :initarg :path :accessor path)))

(defmethod initialize-instance :after ((asset asset) &key)
  (with-slots (path) asset
    (setf path (merge-pathnames path *assets-dir*))))

(defgeneric add-asset (asset)
  (:documentation "Adds the given asset to the appropriate collection."))

(defclass sprite-asset (asset)
  ((dimensions :initarg :dimensions :accessor dimensions)
   (tile-class :initarg :tile-class :accessor tile-class)
   (mappings :initarg :mappings :accessor mappings)))

(defun tile-size (sprite-asset)
  (with-slots (height dimensions tile-class) sprite-asset
    (let ((dim (/ height dimensions)))
      (if (eql tile-class 'button-tile)
	  dim
	  (round (min dim (* dimensions *tile-size*)))))))

(defmethod initialize-instance :after ((sprite-asset sprite-asset) &key)
  (with-slots (path height width surface dimensions) sprite-asset
    (let ((sprite-sheet (sdl-image:load-image path)))
      (setf height (sdl:height sprite-sheet))
      (setf width (sdl:width sprite-sheet))
      (let* ((tile-size (tile-size sprite-asset))
	     (sprite-cells (loop for y from 0 below height by tile-size
				 append (loop for x from 0 below width by width
					      collect (list x y width tile-size)))))
	(setf (sdl:cells sprite-sheet) sprite-cells))
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
    (if asset-instance
	(funcall accessor asset-instance)
	1)))

(defun initargs-list (asset-spec class)
  (closer-mop:finalize-inheritance (find-class class))
  (let ((arg-list (mapcan #'closer-mop:slot-definition-initargs
			  (closer-mop:class-slots (find-class class)))))
    (loop for i in (rest (rest (rest arg-list)))
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
