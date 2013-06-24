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

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "x: ~A y: ~A" x y))))

(defmethod cross ((point point))
  (with-slots (x y) point
    (list
     (make-instance 'point :x x :y (1- y))
     (make-instance 'point :x (1- x) :y y)
     (make-instance 'point :x (1+ x) :y y)
     (make-instance 'point :x x :y(1+ y)))))

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
   (sprite-sheet :initarg :sprite-sheet
		 :accessor sprite-sheet)
   (sprite-cell :initarg :sprite-cell
		:accessor sprite-cell)
   (parent :initarg :parent
	   :accessor parent)
   (action :initarg :action
	   :accessor action
	   :documentation "When not NIL action is executed."))
  (:default-initargs
   :size 1
   :color nil
   :tile-type nil
   :sprite-sheet nil
   :sprite-cell nil
   :action nil
   :parent nil))

(defclass 3x3 ()
  ((coords :initarg :coords
	   :accessor coords
	   :type 'point
	   :documentation "Main ")
   (tiles :initarg :tiles
	  :accessor tiles)
   (tile-type :initarg :tile-type
	      :accessor tile-type)
   (size :initarg :size
	 :accessor size))
  (:default-initargs
   :tiles nil
   :size 9))

(defun init-sprite (path size)
  (let* ((total-size (* size *tile-size*))
	 (sprite-sheet (sdl-image:load-image path))
	 (sprite-cells (loop for y from 0 to total-size by *tile-size*
			     append (loop for x from 0 to size by *tile-size*
					  collect (list x y *tile-size* *tile-size*)))))
    (setf (sdl:cells sprite-sheet) sprite-cells)
    sprite-sheet))

(defun init-sprites ()
  (let* ((sprite-list *sprite-assets*)
	 (indicator (first sprite-list)))
    (loop while sprite-list do
	  (multiple-value-bind (key value tail) (get-properties sprite-list `(,indicator))
	    (setf (gethash indicator *sprites*) (init-sprite (first value) (second value)))
	    (setf sprite-list (cddr tail))
	    (setf indicator (first sprite-list))))))

(defmethod initialize-instance :after ((3x3 3x3) &key)
  (with-slots (tiles coords size tile-type) 3x3
    (let ((rows (sqrt size))
	  (sprite-sheet (gethash tile-type *sprites*)))
      (loop for i below size
	    for x = (floor (mod i rows))
	    for y = 0 then (if (zerop x)
			       (1+ y)
			       y)
	    for tile-coords = (make-instance 'point :x (+ x (x coords)) :y (+ y (y coords)))
	    do (push (make-instance 'tile :coords tile-coords :tile-type tile-type :sprite-sheet sprite-sheet
				    :sprite-cell i :parent 3x3)
		     tiles)))
    (setf (action (nth 4 tiles)) :blow)))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (parent coords color size tile-type sprite-sheet action) object
      (format stream "action: ~A sprite-sheet: ~A parent: ~A x: ~A y: ~A color: ~A size: ~A type: ~A"
	      action sprite-sheet parent (x coords) (y coords) color size tile-type))))

(defgeneric draw (tile))

(defmethod draw ((tile tile))
  (with-slots (coords size color tile-type sprite-sheet sprite-cell) tile
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
	  (setf sprite-cell (getf *road-mapping* (check-road coords) 2)))))))

(defun check-road (coords)
  (with-slots (x y) coords
    (let ((tiles (loop for c in (cross coords)
		       collect (gethash (genhash (x c) (y c)) *entities*))))
      (parse-integer (format nil "~{~A~}" (mapcar #'(lambda (x)
						      (if (and x (equal (tile-type x) :road)) 1 0)) tiles))
		     :radix 2))))

(defmethod draw ((3x3 3x3))
  (with-slots (tiles sprite-sheet) 3x3
    (loop for tile in tiles do
	  (draw tile))))

(defun genhash (&rest rest)
  "Generate hash key based on passed arguments."
  (format nil "~{~a~^,~}" rest))

(defun can-build-p (tiles)
  (let ((ents (loop for tile in tiles
		    collect (gethash (genhash (x (coords tile)) (y (coords tile))) *entities*))))
    (every #'(lambda (tile) (or (eql (tile-type tile) :forest) (eql (tile-type tile) :dirt))) ents)))

(defun blow-up-p (tile)
  (when (and (parent tile) (equal (action tile) :blow))
    (loop for tl in (tiles (parent tile)) do
	  (remhash (gethash (genhash (x (coords tl)) (y (coords tl))) *entities*) *entities*))))

(defgeneric build (entity))

(defmethod build ((tile tile))
  (with-slots (coords tile-type action) tile
    (when (or (equal tile-type :dirt) (can-build-p (list tile)))
      (let ((existing (gethash (genhash (x coords) (y coords)) *entities*)))
	(unless (blow-up-p existing)
	  (setf (gethash (genhash (x coords) (y coords)) *entities*) tile))))))

(defmethod build ((3x3 3x3))
  (with-slots (tiles coords) 3x3
    (when (can-build-p tiles)
      (loop for tile in tiles
       	    do (setf (gethash (genhash (x (coords tile)) (y (coords tile))) *entities*) tile)
	    finally (return t)))))

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
    (build (make-instance 'tile :coords (make-instance 'point :x norm-x :y norm-y)
			  :color (getf *tiles* tile) :tile-type tile :sprite-sheet (gethash tile *sprites*) :sprite-cell 2))))

(defun setup-world ()
  (do-world (i j)
    (let ((coords (make-instance 'point :x j :y i))
	  (tile (random-tile)))
      (setf (gethash (genhash (x coords) (y coords)) *entities*)
	    (make-instance 'tile :coords coords
			   :color (getf *tiles* tile) :tile-type tile)))))

(defun reset ()
  (setf *entities* (make-hash-table :test #'equal))
  (setup-world))

