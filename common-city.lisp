(in-package #:common-city)

(defparameter *sprites* (make-hash-table :test #'equal))
(defparameter *buttons* (make-hash-table :test #'equal))

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
			      :powerplant complex-tile
			      :road sprite-tile))

(defclass entity ()
  ((x :initarg :x :accessor x :documentation "X coordinate.")
   (y :initarg :y :accessor y :documentation "Y coordinate.")
   (size :initarg :size :accessor size :documentation "Entity size.")
   (surface :initarg :surface :accessor surface))
  (:default-initargs
   :surface sdl:*default-display*))

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "x: ~A y: ~A" x y))))

(defclass button (entity)
  ((image-path :initarg :image-path :accessor image-path))
  (:default-initargs
   :x 0
   :y 0
   :size 42))

(defclass sprite-tile (entity)
  ((parent :initarg :parent :accessor parent)
   (action :initarg :action :accessor action)
   (tile-type :initarg :tile-type :accessor tile-type)
   (sprite-cell :initarg :sprite-cell :accessor sprite-cell)
   (sprite-sheet :initarg :sprite-sheet :accessor sprite-sheet))
  (:default-initargs
   :size 1
   :sprite-cell nil
   :sprite-sheet nil
   :parent nil
   :action nil))

(defmethod print-object ((object sprite-tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (tile-type sprite-cell sprite-sheet) object
      (format stream "sprite-cell: ~A sprite-sheet: ~A tile-type: ~A" sprite-cell sprite-sheet tile-type))))

(defun no-water ()
  (let ((wild (loop for i from 21 below 38
		    collect i)))
    (nth (random (length wild)) wild)))

(defmethod initialize-instance :after ((entity sprite-tile) &key)
  (with-slots (tile-type sprite-sheet sprite-cell) entity
    (setf sprite-sheet (gethash tile-type *sprites*))
    (case tile-type
      (:wilderness (setf sprite-cell (no-water)))
      (:dirt (progn
	       (setf sprite-sheet (gethash :wilderness *sprites*))
	       (setf sprite-cell 0)))
      (:road (setf sprite-cell 2)))
    (build entity)))

(defclass complex-tile (entity)
  ((tiles :initarg :tiles :accessor tiles :documentation "List of tiles for this entity."))
  (:default-initargs
   :size 9
   :tiles '()))

(defmethod print-object ((object complex-tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y size) object
      (format stream "~A ~A size: ~A" x y size))))

(defmethod initialize-instance :after ((entity complex-tile) &key tile-type)
  (with-slots (x y size tiles sprite-sheet) entity
    (setf size (sprite-dimensions tile-type))
    (when (can-build-p entity)
      (let ((rows (sqrt size))
	    (sprite-sheet (gethash tile-type *sprites*))
	    (action-cell (1- (ceiling (/ size 2)))))
	(setf sprite-sheet sprite-sheet)
	(loop for i below size
	      for nx = (floor (mod i rows))
	      for ny = 0 then (if (zerop nx)
				  (1+ ny)
				  ny)
	      do (push (make-instance 'sprite-tile :x (+ nx x) :y (+ ny y)
				      :sprite-cell i :tile-type tile-type
				      :parent entity) tiles))
	(setf (action (nth action-cell tiles)) :blow))
      (build entity))))

(defun with-tile-size-at (size x y)
  (let ((rows (sqrt size)))
    (loop for i below size
	  for nx = (floor (mod i rows))
	  for ny = 0 then (if (zerop nx)
			      (1+ ny)
			      ny)
	  collect `(,(+ nx x) ,(+ ny y)))))

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

(defun init-buttons ()
  (loop for indicator in *button-assets* by #'cddr do
	(multiple-value-bind (key value tail) (get-properties *button-assets* `(,indicator))
	  (setf (gethash indicator *buttons*) (sdl:load-image (first value))))))

(defgeneric draw (entity))

(defmethod draw ((entity button))
  (with-slots (x y image-path surface) entity
    (sdl:draw-surface-at-* image-path x y :surface surface)))

(defmethod draw ((entity sprite-tile))
  (with-slots (x y size sprite-cell sprite-sheet tile-type) entity
    (let ((x (* x *tile-size*))
	  (y (* y *tile-size*)))
      (sdl:draw-surface-at-* sprite-sheet x y :cell sprite-cell)
      (when (equal tile-type :road)
	(setf sprite-cell (getf *road-mapping* (check-road entity) 2))))))

(defmethod draw ((entity complex-tile))
  (with-slots (tiles) entity
    (loop for tile in tiles do
	  (draw tile))))

(defun blow-up-p (tile)
  (when tile
    (if (and (parent tile) (equal (action tile) :blow))
	(remove-entity (parent tile))
	(remove-entity tile))))

(defgeneric can-build-p (entity)
  (:documentation "Checks if an entity can be build."))

(defmethod can-build-p ((entity sprite-tile))
  (with-slots (x y tile-type) entity
    (let* ((existing-tile (gethash (genhash x y) *entities*)))
      (if existing-tile
	  (or (eql tile-type :dirt) (member (tile-type existing-tile) '(:dirt :wilderness)))
	  t))))

(defmethod can-build-p ((entity complex-tile))
  (with-slots (size x y) entity
    (let* ((tile-coords (with-tile-size-at size x y))
	   (existing-tiles (loop for coords in tile-coords
				 collect (gethash (genhash (first coords) (second coords)) *entities*))))
      (every #'can-build-p existing-tiles))))

(defgeneric build (entity)
  (:documentation "Adds entity to the `*entities*' collection."))

(defmethod build ((entity button))
  (with-slots (image-path) entity
    (setf (gethash image-path *entities*) entity)))

(defmethod build ((entity sprite-tile))
  (when (can-build-p entity)
    (with-slots (x y) entity
      (setf (gethash (genhash x y) *entities*) entity))))

(defgeneric remove-entity (entity)
  (:documentation "Removing an entity means transforming into the base tile :dirt."))

(defmethod remove-entity ((entity sprite-tile))
  (with-slots (x y) entity
    (build (make-instance 'sprite-tile :x x :y y :tile-type :dirt))))

(defmethod remove-entity ((entity complex-tile))
  (with-slots (tiles) entity
    (mapc #'remove-entity tiles)))

(defun dozer (x y)
  (multiple-value-bind (hashval norm-x norm-y) (snap-to-tile x y)
    (declare (ignore hashval))
    (let ((existing-tile (gethash (genhash norm-x norm-y) *entities*)))
      (blow-up-p existing-tile))))

(defun build-tile (x y tile-type)
  "Create a tile instance dispatched on tile-type with normalized coords."
  (multiple-value-bind (hashval norm-x norm-y) (snap-to-tile x y)
    (declare (ignore hashval))
    (make-instance (getf *tiles* tile-type) :x norm-x :y norm-y :tile-type tile-type)))

(defclass point ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)))

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

(defmacro do-world ((i j) &body body)
  `(loop for ,i below (/ *map-height* *tile-size*)
	 do (loop for ,j below (/ *map-width* *tile-size*)
		  do ,@body)))

(defun snap-to-tile (x y)
  "Adjust mouse coords to tile coords."
  (let ((norm-x (/ (- x (mod x *tile-size*)) *tile-size*))
	(norm-y (/ (- y (mod y *tile-size*)) *tile-size*)))
    (values (gethash (genhash norm-x norm-y) *entities*) norm-x norm-y)))

(defun setup-world ()
  (do-world (i j)
    (build (make-instance 'sprite-tile :x j :y i :tile-type :wilderness))))

(defun setup-menu ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (build (make-instance 'button :x 0 :y 0 :image-path v :surface *menu-surface*))) *buttons*))

(defun reset ()
  (setf *entities* (make-hash-table :test #'equal))
  (setup-world)
  (setup-menu))
