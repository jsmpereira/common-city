;;; -*- Mode: Lisp -*-

(in-package :common-city)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)
(defparameter *map-width* (* 54 *tile-size*))
(defparameter *map-height* (* 45 *tile-size*))
(defparameter *menu-width* (- *screen-width* *map-width*))
(defparameter *bg-color* (sdl:color :r 136 :g 136 :b 136))
(defparameter *build-color* sdl:*blue*)

(defparameter *map-cursor* :residential)
(defparameter *menu-surface* nil)
(defparameter *map-surface* nil)
(defparameter *text-surface* nil)

(defclass surface ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (surface :initarg :surface :accessor surface)
   (color :initarg :color :accessor color)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:default-initargs
   :color *bg-color*))

(defmethod initialize-instance :after ((surface surface) &key)
  (with-slots (width height surface) surface
    (setf surface (sdl:create-surface width height))))

(defmethod draw ((surface surface))
  (with-slots (x y surface width height color) surface
    (sdl:draw-surface-at-* surface x y)
    (sdl:fill-surface color :surface surface)
    (sdl:draw-rectangle-* 0 0 width height :color sdl:*black* :surface surface)
    (sdl:draw-rectangle-* 1 1 width height :color sdl:*white* :surface surface)))

(defun hit-test (button mx my)
  (with-slots (x y size sprite-sheet) button
    (let ((x (+ x *map-width*))
	  (height (/ (sdl:height sprite-sheet) size)))
      (and (> mx x) (< mx (+ x (sdl:width sprite-sheet))) (> my y) (< my (+ y height))))))

(defun play-sound (asset)
  (let ((sound (sdl-mixer:load-sample (getf *audio-assets* asset))))
    (sdl-mixer:play-sample sound)))

(defun render-step ()
  (loop for k being the hash-keys in *entities* using (hash-value v)
	do (draw v)))

(defun menu ()
  (maphash #'(lambda (k v)
	       (draw v)
	       (unless (eql *map-cursor* (tile-type v))
		 (setf (sprite-cell v) 0))
	       (when (and (hit-test v (sdl:mouse-x) (sdl:mouse-y)) (sdl:mouse-left-p))
		 (setf *map-cursor* (tile-type v))
		 (setf (sprite-cell v) 1))) *buttons*))

(defun setup-surfaces ()
  (setf *map-surface* (make-instance 'surface :x 0 :y 0 :width *map-width* :height *map-height*))
  (setf *menu-surface* (make-instance 'surface :x *map-width* :y 0 :width *menu-width* :height *screen-height*))
  (setf *text-surface* (make-instance 'surface :x 0 :y *map-height* :width *map-width* :height (- *screen-height* *map-height*))))

(defun render ()
  (draw *map-surface*)
  (draw *menu-surface*)
  (draw *text-surface*)
  (render-step)
  (menu)
  (cursor))

(defun handle-keys (key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))
    (:sdl-key-x (reset))
    (:sdl-key-d (setf *map-cursor* :dozer))
    (:sdl-key-h (setf *map-cursor* :residential))
    (:sdl-key-c (setf *map-cursor* :commercial))
    (:sdl-key-n (setf *map-cursor* :nuclear))
    (:sdl-key-r (setf *map-cursor* :road))
    (:sdl-key-t (setf *map-cursor* :rail))
    (:sdl-key-w (setf *map-cursor* :wire))
    (:sdl-key-g (setf *map-cursor* :garden))
    (:sdl-key-i (setf *map-cursor* :industrial))
    (:sdl-key-f (setf *map-cursor* :fire-department))
    (:sdl-key-p (setf *map-cursor* :police-department)))
  (setf (sprite-cell (gethash *map-cursor* *buttons*)) 1))

(defun handle-mouse (x y)
  (when (sdl:mouse-left-p)
    (case *map-cursor*
      (:dozer
       (dozer x y)
       (play-sound :dozer))
      (t
       (when (inside-map-p)
	 (build-tile x y *map-cursor*))))))

(defun inside-map-p ()
  "FIXME - duplicate code"
  (multiple-value-bind (tile mx my) (snap-to-tile (sdl:mouse-x) (sdl:mouse-y))
    (declare (ignore tile))
    (let* ((mx (* mx *tile-size*))
	   (my (* my *tile-size*))
	   (cursor-size (or (asset-data *map-cursor* 'dimensions) 1))
	   (dimensions (sdl:cast-to-int (* (sqrt cursor-size) *tile-size*))))
      (and (<= (+ mx dimensions) *map-width*) (<= (+ my dimensions) *map-height*)))))

(defun cursor ()
  (draw-tooltip (asset-data *map-cursor* 'tooltip :collection *button-assets*)
		(asset-data *map-cursor* 'cost :collection *button-assets*))
  (multiple-value-bind (tile mx my) (snap-to-tile (sdl:mouse-x) (sdl:mouse-y))
    (when tile
      (let* ((mx (* mx *tile-size*))
	     (my (* my *tile-size*))
	     (cursor-size (asset-data *map-cursor* 'dimensions))
	     (dimensions (sdl:cast-to-int (* (sqrt cursor-size) *tile-size*))))
	(if (and (< mx *map-width*) (< my *map-height*))
	    (with-slots (x y) tile
	      (let ((x (* x *tile-size*))
		    (y (* y *tile-size*)))
		(sdl:with-color (col *build-color*)
		  (sdl:draw-rectangle-* x y dimensions dimensions :surface (surface *map-surface*)))))
	    (progn
	      (sdl:draw-circle-* (sdl:mouse-x) (sdl:mouse-y) 5)))))))

(defun main ()
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
      (sdl:initialise-default-font)
      (sdl:window *screen-width* *screen-height*
		  :title-caption "Common City"
		  :icon-caption "Common City"
		  :double-buffer t)
      (setf (sdl:frame-rate) 30)
      (sdl-mixer:open-audio)
      (sdl:enable-key-repeat nil nil)
      (setf sdl-cffi::*image-loaded-p* t) ; hack to load .png
      (sdl:init-image :png)
   ;   (sdl:show-cursor nil)
    ;  (sdl:sdl-wm-grab-input :SDL-GRAB-ON)
      (setup-surfaces)
      (setup-assets)
      (reset)
      (sdl:with-events ()
	(:quit-event ()
		     (sdl-mixer:close-audio)
		     (sdl:quit-image)
		     t)
	(:key-down-event (:key key)
			 (handle-keys key))
	(:mouse-motion-event (:state s :x-rel dx :y-rel dy :x x :y y))
	(:mouse-button-down-event (:button button :state state :x x :y y))
	(:idle ()
	       (sdl:clear-display *bg-color*)
	       (handle-mouse (sdl:mouse-x) (sdl:mouse-y))
	       (let ((connection
		      (or swank::*emacs-connection* (swank::default-connection))))
		 (when (and connection (not (eql swank:*communication-style* :spawn)))
		   (swank::handle-requests connection t)))
	       (render)
	       (sdl:update-display))))))
