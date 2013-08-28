;;; -*- Mode: Lisp -*-

(in-package :common-city)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)
(defparameter *bg-color* sdl:*black*)

(defparameter *cursor* :dozer)

(defparameter *assets-dir* 
  (merge-pathnames #P"assets/" simcity-config:*base-directory*))

(defparameter *sprite-assets*
  `(:residential (,(merge-pathnames "residential.png" *assets-dir*) 9)
		 :commercial (,(merge-pathnames "commercial.png" *assets-dir*) 9)
		 :road (,(merge-pathnames "road.png" *assets-dir*) 15)
		 :wilderness (,(merge-pathnames "wilderness.png" *assets-dir*) 38)))

(defun sprite-dimensions (key)
  "Return sprite dimensions."
  (multiple-value-bind (key value tail) (get-properties *sprite-assets* `(,key))
    (declare (ignore key tail))
    (if value
      (second value)
      1)))

(defparameter *audio-assets*
  `(:dozer ,(merge-pathnames "rumble.wav" *assets-dir*)
	   :bop ,(merge-pathnames "bop.wav" *assets-dir*)))

(defparameter *sprite-sheet* nil)

 (defun play-sound (asset)
   (let ((sound (sdl-mixer:load-sample (getf *audio-assets* asset))))
     (sdl-mixer:play-sample sound)))

(defun render-step ()
  (loop for k being the hash-keys in *entities* using (hash-value v)
	do (draw v)))

(defun render ()
  (render-step)
  (cursor))

(defun handle-keys (key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))
    (:sdl-key-x (reset))
    (:sdl-key-d (setf *cursor* :dozer))
    (:sdl-key-r (setf *cursor* :residential))
    (:sdl-key-c (setf *cursor* :commercial))
    (:sdl-key-t (setf *cursor* :road))))

(defun handle-mouse ()
  (when (and (sdl:mouse-left-p); (sdl:mouse-released-p button)
	     )
    (case *cursor*
      (:dozer
       (dozer (sdl:mouse-x) (sdl:mouse-y))
       (play-sound :dozer))
      (:road
       (build-tile (sdl:mouse-x) (sdl:mouse-y) :road))
      (t
       (build-tile (sdl:mouse-x) (sdl:mouse-y) *cursor*)))))

(defun cursor ()
  (let ((tile (snap-to-tile (sdl:mouse-x) (sdl:mouse-y))))
    (when tile
      (with-slots (x y) tile
	(let ((x (* x *tile-size*))
	      (y (* y *tile-size*))
	      (dimensions (floor (* (sqrt (sprite-dimensions *cursor*)) *tile-size*))))
	  (sdl:draw-string-solid-* (format nil "(~A, ~A)" x y) x y)
	  (case *cursor*
	    (:dozer
	     (sdl:draw-rectangle-* x y *tile-size* *tile-size* :color sdl:*white*))
	    (:road
	     (sdl:draw-rectangle-* x y *tile-size* *tile-size* :color sdl:*black*))
	    (t
	     (sdl:draw-rectangle-* x y dimensions dimensions :color sdl:*blue*))))))))

(defun main ()
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
      (sdl:initialise-default-font)
      (sdl:window *screen-width* *screen-height*
		  :title-caption "K SimCity"
		  :double-buffer t)
      (setf (sdl:frame-rate) 30)
      (sdl-mixer:open-audio)
      (sdl:enable-key-repeat nil nil)
      (setf sdl-cffi::*image-loaded-p* t) ; hack to load .png
      (sdl:init-image :png)
      (init-sprites)
      (reset)
      (sdl:with-events ()
	(:quit-event ()
		     (sdl-mixer:close-audio)
		     (sdl:quit-image)
		     t)
	(:key-down-event (:key key)
			 (handle-keys key))
	(:mouse-motion-event (:state s :x-rel dx :y-rel dy :x x :y y))
	(:mouse-button-up-event (:button button :state state :x x :y y)
					;(handle-mouse state)
				)
	(:idle ()
	       (sdl:clear-display *bg-color*)
	       (handle-mouse)
	       (let ((connection
		      (or swank::*emacs-connection* (swank::default-connection))))
		 (when (and connection (not (eql swank:*communication-style* :spawn)))
		   (swank::handle-requests connection t)))
	       (render)
	       (sdl:update-display))))))

