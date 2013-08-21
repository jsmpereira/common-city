(in-package #:common-city)

(defparameter *sprites* (make-hash-table :test #'equal))

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

(defparameter *entities* (make-hash-table :test #'equal))

(defparameter *tile-size* 16)
(defparameter *tiles* `(:dirt sprite-tile
			      :forest sprite-tile
			      :residential complex-tile
			      :commercial complex-tile
			      :road sprite-tile))

(defclass entity ()
  ((x :initarg :x :accessor x :documentation "X coordinate.")
   (y :initarg :y :accessor y :documentation "Y coordinate.")
   (size :initarg :size :accessor size :documentation "Entity size.")))

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "x: ~A y: ~A" x y))))

(defclass sprite-tile (entity)
  ((tile-type :initarg :tile-type :accessor tile-type)
   (sprite-cell :initarg :sprite-cell :accessor sprite-cell)
   (sprite-sheet :initarg :sprite-sheet :accessor sprite-sheet)
   (size))
  (:default-initargs
   :sprite-cell nil
   :sprite-sheet nil))

(defmethod print-object ((object sprite-tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (tile-type sprite-cell sprite-sheet) object
      (format stream "sprite-cell: ~A sprite-sheet: ~A tile-type: ~A" sprite-cell sprite-sheet tile-type))))

(defun no-water ()
  (let ((wild (loop for i from 21 below 38
		    collect i)))
    (nth (random (length wild)) wild)))

(defmethod initialize-instance :after ((entity sprite-tile) &key)
  (with-slots (size tile-type sprite-sheet sprite-cell) entity
    (setf sprite-sheet (gethash tile-type *sprites*))
    (case tile-type
      (:wilderness (setf sprite-cell (no-water)))
      (:dirt (progn
	       (setf sprite-sheet (gethash :wilderness *sprites*))
	       (setf sprite-cell 0)))
      (:road (setf sprite-cell 2))))
  (build entity))

(defclass complex-tile (entity)
  ((size)
   (tiles :initarg :tiles :accessor tiles :documentation "List of tiles for this entity."))
  (:default-initargs
   :size 9
   :tiles '()))

(defmethod print-object ((object complex-tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y size) object
      (format stream "~A ~A size: ~A" x y size))))

(defmethod initialize-instance :after ((entity complex-tile) &key tile-type)
  (with-slots (x y size tiles sprite-sheet) entity
    (let ((rows (sqrt size))
	  (sprite-sheet (gethash tile-type *sprites*)))
      (setf sprite-sheet sprite-sheet)
      (loop for i below size
	    for nx = (floor (mod i rows))
	    for ny = 0 then (if (zerop nx)
				(1+ ny)
				ny)
	    do (push (make-instance 'sprite-tile :x (+ nx x) :y (+ ny y) :sprite-cell i :tile-type tile-type) tiles))))
  (build entity))

(defun init-sprite (path size)
  (let* ((total-size (* size *tile-size*))
	 (sprite-sheet (sdl-image:load-image path))
	 (sprite-cells (loop for y from 0 to total-size by *tile-size*
			     append (loop for x from 0 to size by total-size
					  collect (list x y *tile-size* *tile-size*)))))
    (setf (sdl:cells sprite-sheet) sprite-cells)
    sprite-sheet))

(defun init-sprites ()
  (loop for indicator in *sprite-assets* by #'cddr do
	(multiple-value-bind (key value tail) (get-properties *sprite-assets* `(,indicator))
	  (setf (gethash indicator *sprites*) (init-sprite (first value) (second value))))))

(defgeneric draw (entity))

(defmethod draw ((entity sprite-tile))
  (with-slots (x y size sprite-cell sprite-sheet tile-type) entity
    (let ((x (* x *tile-size*))
	  (y (* y *tile-size*)))
      ;; (when color
      ;; 	(sdl:draw-box-* x y size size :color color))
      (sdl:draw-line-* x y x y :color sdl:*black*)
      (when sprite-sheet
	(sdl:draw-surface-at-* sprite-sheet x y :cell sprite-cell))
      (when (equal tile-type :road)
	(setf sprite-cell (getf *road-mapping* (check-road entity) 2))))))

(defmethod draw ((entity complex-tile))
  (with-slots (tiles) entity
    (loop for tile in tiles do
	  (draw tile))))

(defgeneric build (entity)
  (:documentation "Adds entity to the `*entities*' collection."))

(defmethod build ((tile tile))
  (with-slots (coords tile-type action) tile
    (when (or (equal tile-type :dirt) (can-build-p (list tile)))
      (let ((existing (gethash (genhash (x coords) (y coords)) *entities*)))
	(unless (blow-up-p existing)
	  (setf (gethash (genhash (x coords) (y coords)) *entities*) tile))))))

(defmethod build ((entity sprite-tile))
  (with-slots (x y tile-type) entity
    (when (or (eql tile-type :wilderness) (can-build-p (list entity)))
      (setf (gethash (genhash x y) *entities*) entity))))

(defmethod build ((entity complex-tile))
  (with-slots (x y tiles) entity
    (when (can-build-p tiles)
      (loop for tile in tiles
	    do (setf (gethash (genhash (x tile) (y tile)) *entities*) tile)
	    finally (return t)))))

(defun build-tile (x y tile-type)
  (multiple-value-bind (hashval norm-x norm-y) (snap-to-tile x y)
    (declare (ignore hashval))
    (make-instance (getf *tiles* tile-type) :x norm-x :y norm-y :tile-type tile-type)))

(defmethod cross ((entity entity))
  (with-slots (x y) entity
    (list
     (make-instance 'point :x x :y (1- y))
     (make-instance 'point :x (1- x) :y y)
     (make-instance 'point :x (1+ x) :y y)
     (make-instance 'point :x x :y(1+ y)))))

(defun check-road (entity)
  (with-slots (x y) entity
    (let ((tiles (loop for c in (cross entity)
		       collect (gethash (genhash (x c) (y c)) *entities*))))
      (parse-integer (format nil "~{~A~}" (mapcar #'(lambda (x)
						      (if (and x (equal (tile-type x) :road)) 1 0)) tiles))
		     :radix 2))))

(defun genhash (&rest rest)
  "Generate hash key based on passed arguments."
  (format nil "~{~a~^,~}" rest))

(defun can-build-p (tiles)
  (let ((ents (loop for tile in tiles
		    collect (gethash (genhash (x tile) (y tile)) *entities*))))
    (every #'(lambda (tile) (or (eql (tile-type tile) :wilderness) (eql (tile-type tile) :dirt))) ents)))

(defun blow-up-p (tile)
  (when (and (parent tile) (equal (action tile) :blow))
    (loop for tl in (tiles (parent tile)) do
	  (remhash (gethash (genhash (x (coords tl)) (y (coords tl))) *entities*) *entities*))))

(defmacro do-world ((i j) &body body)
  `(loop for ,i below (/ *screen-height* *tile-size*)
	 do (loop for ,j below (/ *screen-width* *tile-size*)
		  do ,@body)))

(defun snap-to-tile (x y)
  (let ((norm-x (/ (- x (mod x *tile-size*)) *tile-size*))
	(norm-y (/ (- y (mod y *tile-size*)) *tile-size*)))
    (values (gethash (genhash norm-x norm-y) *entities*) norm-x norm-y)))

(defun setup-world ()
  (do-world (i j)
    (setf (gethash (genhash j i) *entities*)
	  (make-instance 'sprite-tile :x j :y i :tile-type :wilderness))))

(defun reset ()
  (setf *entities* (make-hash-table :test #'equal))
  (setup-world))
