;;;; simcity.lisp

(in-package #:simcity)

;;; "simcity" goes here. Hacks and glory await!

(defparameter *entities* nil)

(defparameter *tile-size* 20)
(defparameter *tiles* `(:dirt ,(sdl:color :r 150 :g 100 :b 50)
			      :forest ,(sdl:color :r 1 :g 100 :b 0)
			      :residencial ,(sdl:color :r 24 :g 63 :b 219)
			      :road ,(sdl:color :r 0 :g 0 :b 0)))
(defparameter *world* nil)

(defclass tile ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (size :initarg :size
	 :accessor size)
   (color :initarg :color
	  :accessor color)
   (tile-type :initarg :tile-type
	      :accessor tile-type))
  (:default-initargs
   :size *tile-size*
   :tile-type :dirt))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y size color size tile-type) object
      (format stream "x: ~a y: ~a color: ~a size: ~a type: ~a" x y color size tile-type))))

(defgeneric draw (tile &key scale))

(defmethod draw ((tile tile) &key (scale 1))
  (with-slots (x y size color tile-type) tile
    (sdl:draw-box-* x y (* size scale) (* size scale) :color color)
    (sdl:draw-line-* x y x y :color sdl:*black*)
    (when (equal tile-type :road)
      (sdl:draw-line-* (+ x (/ size 2)) (+ y (/ size 4)) (+ x (/ size 2)) y :color sdl:*white*))))

(defmacro do-world ((i j) &body body)
  `(loop for ,i below (array-dimension *world* 0)
	 do (loop for ,j below (array-dimension *world* 1)
		  do ,@body)))

(defun random-tile ()
  (nth (random 2) 
       (loop for i in *tiles* by #'cddr
	     collect i)))

(defun snap-to-tile (x y)
  (let ((tile nil)
	(coord-x nil)
	(coord-y nil))
    (do-world (i j)
      (with-accessors ((tile-x x) (tile-y y)) (aref *world* i j)
	(when (tile-bounds tile-x tile-y x y)
	  (setf tile (aref *world* i j))
	  (setf coord-x i)
	  (setf coord-y j))))
    (values tile coord-x coord-y)))

(defun tile-bounds (tile-x tile-y x y)
  (and (>= x tile-x) (<= x (+ tile-x *tile-size*))
       (>= y tile-y) (<= y (+ tile-y *tile-size*))))

(defun simple-tile (x y tile)
  (multiple-value-bind (to-replace coord-x coord-y) (snap-to-tile x y)
    (with-accessors ((tile-x x) (tile-y y)) to-replace
      (setf (aref *world* coord-x coord-y) (make-instance 'tile
							  :x tile-x
							  :y tile-y
							  :color (getf *tiles* tile)
							  :tile-type tile)))))

(defclass residential ()
  ((tiles :initarg :tiles
	  :accessor tiles)
   (color :initarg :color
	  :accessor color)
   (top-left :initarg :top-left
	     :accessor top-left)))

(defun get-n-coords (x y n)
  (loop for i from 0 below (sqrt n) do
	(loop for j from 0 below (sqrt n)
	      collect i)))

(defun get-n-tiles (x y n)
  (let ((tiles nil))
    (push (aref *world* (+ x 1) y) tiles)
    (push (aref *world* (+ x 2) y) tiles)
    ;line 2
    (push (aref *world* x (+ y 1)) tiles)
    (push (aref *world* (+ x 1) (+ y 1)) tiles)
    (push (aref *world* (+ x 2) (+ y 1)) tiles)
    ;line 3
    (push (aref *world* x (+ y 2)) tiles)
    (push (aref *world* (+ x 1) (+ y 2)) tiles)
    (push (aref *world* (+ x 2) (+ y 2)) tiles)))

(defun can-build? (tiles)
  (every #'(lambda (tile) (or (eql (tile-type tile) :forest) (eql (tile-type tile) :dirt))) tiles))

(defun residential (x y)
  (let ((res nil))
    (multiple-value-bind (top-left-tile coord-x coord-y) (snap-to-tile x y)
      (setf res (get-n-tiles coord-x coord-y 8))
      (push top-left-tile res)
      (when (can-build? res)
	(loop for tile in res do
	      (setf (tile-type tile) :residential)
	      (setf (color tile) (getf *tiles* :dirt)))
	(push (make-instance 'residential :tiles res
			     :color sdl:*blue*
			     :top-left top-left-tile) *entities*)))))

(defun draw-residential (residential)
  (with-slots (top-left color tiles) residential
    (loop for tile in tiles do
	  (draw tile))
    (sdl:draw-rectangle-* (x top-left) (y top-left) 60 60 :color color)))

(defun setup-world ()
  (do-world (i j)
    (setf (aref *world* i j) (make-instance 'tile
					    :x (* j *scale*)
					    :y (* i *scale*)
					    :color (getf *tiles* (random-tile))))))

(defun reset ()
  (setf *world* (make-array '(30 40) :element-type 'tile))
  (setup-world)
  (setf *entities* nil))

