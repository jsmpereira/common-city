(in-package #:common-city)

(defparameter *entities* (make-hash-table :test #'equal))

(defparameter *tile-size* 16)
(defparameter *tiles* `(:dirt ,(sdl:color :r 150 :g 100 :b 50)
			      :forest ,(sdl:color :r 1 :g 100 :b 0)
			      :residential ,(sdl:color :r 0 :g 230 :b 0)
			      :commercial ,(sdl:color :r 102 :g 102 :b 230)
			      :road ,(sdl:color :r 0 :g 0 :b 0)))

(defclass point ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)))

(defclass tile ()
  ((coords :initarg :coords
	   :accessor coords
	   :type 'point
	   :documentation "Top left (x,y) tuple.")
   (size :initarg :size
	 :accessor size)
   (color :initarg :color
	  :accessor color)
   (tile-type :initarg :tile-type
	      :accessor tile-type)
   (sprite-cell :initarg :sprite-cell
		:accessor sprite-cell))
  (:default-initargs
   :size 1
   :color nil
   :tile-type nil
   :sprite-cell nil))

(defclass 3x3 (tile)
  ((tiles :initarg :tiles
	  :accessor tiles)
   (sprite-sheet :initarg :sprite-sheet
		 :accessor sprite-sheet))
  (:default-initargs
   :tiles nil
   :sprite-sheet nil
   :size 9))

(defmethod init-sprite ((3x3 3x3))
  (with-slots (size tile-type sprite-sheet) 3x3
    (let* ((total-size (* size *tile-size*))
	   (sprite-cells (loop for y from 0 to total-size by *tile-size*
			       append (loop for x from 0 to size by *tile-size*
					    collect (list x y *tile-size* *tile-size*)))))
      (setf sprite-sheet (sdl-image:load-image (getf *image-assets* tile-type)))
      (setf (sdl:cells sprite-sheet) sprite-cells))))

(defmethod initialize-instance :after ((3x3 3x3) &key)
  (with-slots (tiles coords size tile-type) 3x3
    (let ((rows (sqrt size)))
      (loop for i below size
	    for x = (floor (mod i rows))
	    for y = 0 then (if (zerop x)
			       (1+ y)
			       y)
	    for tile-coords = (make-instance 'point :x (+ x (x coords)) :y (+ y (y coords)))
	    do (push (make-instance 'tile :coords tile-coords :tile-type tile-type
				    :sprite-cell i)
		     tiles)))
    (init-sprite 3x3)))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (coords color size tile-type) object
      (format stream "x: ~A y: ~A color: ~A size: ~A type: ~A" (x coords) (y coords) color size tile-type))))

(defgeneric draw (entity &key &allow-other-keys))

(defmethod draw ((tile tile) &key sprite-sheet)
  (with-slots (coords size color tile-type sprite-cell) tile
    (with-slots (x y) coords
      (let ((size (* size *tile-size*))
	    (x (* x *tile-size*))
	    (y (* y *tile-size*)))
	(when color
	  (sdl:draw-box-* x y size size :color color))
	(sdl:draw-line-* x y x y :color sdl:*black*)
	(when sprite-sheet
	  (sdl:draw-surface-at-* sprite-sheet x y :cell sprite-cell))
	(when (equal tile-type :road)
	  (sdl:draw-line-* (+ x (/ size 2)) (+ y (/ size 4)) (+ x (/ size 2)) y :color sdl:*white*))))))

(defmethod draw ((3x3 3x3) &key)
  (with-slots (tiles sprite-sheet) 3x3
    (loop for tile in tiles do
	  (draw tile :sprite-sheet sprite-sheet))))

(defun genhash (&rest rest)
  "Generate hash key based on passed arguments."
  (format nil "~{~a~^,~}" rest))

(defun can-build-p (tiles)
  (let ((ents (loop for tile in tiles
		    collect (gethash (genhash (x (coords tile)) (y (coords tile))) *entities*))))
    (every #'(lambda (tile) (or (eql (tile-type tile) :forest) (eql (tile-type tile) :dirt))) ents)))

(defgeneric build (entity))

(defmethod build ((3x3 3x3))
  (with-slots (tiles coords) 3x3
    (when (can-build-p tiles)
      (loop for tile in tiles
	    do (setf (gethash (genhash (x (coords tile)) (y (coords tile))) *entities*) tile))
      (setf (gethash (genhash (x coords) (y coords)) *entities*) 3x3))))

(defun build-3x3 (x y tile-type)
  (multiple-value-bind (hashval norm-x norm-y) (snap-to-tile x y)
    (declare (ignore hashval))
    (build (make-instance '3x3 :coords (make-instance 'point :x norm-x :y norm-y) :tile-type tile-type))))

(defmacro do-world ((i j) &body body)
  `(loop for ,i below (/ *screen-height* *tile-size*)
	 do (loop for ,j below (/ *screen-width* *tile-size*)
		  do ,@body)))

(defun random-tile ()
  (nth (random 2) 
       (loop for i in *tiles* by #'cddr
	     collect i)))

(defun snap-to-tile (x y)
  (let ((norm-x (/ (- x (mod x *tile-size*)) *tile-size*))
	(norm-y (/ (- y (mod y *tile-size*)) *tile-size*)))
    (values (gethash (genhash norm-x norm-y) *entities*) norm-x norm-y)))

(defun simple-tile (x y tile)
  (multiple-value-bind (hashval norm-x norm-y) (snap-to-tile x y)
    (declare (ignore hashval))
    (setf (gethash (genhash norm-x norm-y) *entities*) 
	  (make-instance 'tile :coords (make-instance 'point :x norm-x :y norm-y)
			 :color (getf *tiles* tile) :tile-type tile))))

(defun setup-world ()
  (do-world (i j)
    (let ((coords (make-instance 'point :x j :y i))
	  (tile (random-tile)))
      (setf (gethash (genhash (x coords) (y coords)) *entities*)
	    (make-instance 'tile :coords coords
			   :color (getf *tiles* tile) :tile-type tile )))))

(defun reset ()
  (setf *entities* (make-hash-table :test #'equal))
  (setup-world))

