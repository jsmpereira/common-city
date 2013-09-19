;;; -*- Mode: Lisp -*-

(in-package :common-city)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)
(defparameter *menu-width* 200)
(defparameter *map-width* ( - *screen-width* *menu-width*))
(defparameter *map-height* *screen-height*)
(defparameter *bg-color* (sdl:color :r 136 :g 136 :b 136))
(defparameter *build-color* sdl:*blue*)

(defparameter *cursor* :residential)
(defparameter *menu-surface* nil)

(defun play-sound (asset)
  (let ((sound (sdl-mixer:load-sample (getf *audio-assets* asset))))
    (sdl-mixer:play-sample sound)))

(defun render-step ()
  (loop for k being the hash-keys in *entities* using (hash-value v)
	do (draw v)))

(defun menu ()
  (sdl:draw-surface-at-* *menu-surface* *map-width* 0)
  (sdl:clear-display *bg-color* :surface *menu-surface*)
  (sdl:draw-rectangle-* 0 0 *menu-width* *screen-height* :color sdl:*black* :surface *menu-surface*)
  (sdl:draw-rectangle-* 1 1 *menu-width* *screen-height* :color sdl:*white* :surface *menu-surface*))

(defun render ()
  (render-step)
  (cursor)
  (menu))

(defun handle-keys (key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))
    (:sdl-key-x (reset))
    (:sdl-key-d (setf *cursor* :dozer))
    (:sdl-key-h (setf *cursor* :residential))
    (:sdl-key-c (setf *cursor* :commercial))
    (:sdl-key-n (setf *cursor* :nuclear))
    (:sdl-key-r (setf *cursor* :road))
    (:sdl-key-w (setf *cursor* :wire))
    (:sdl-key-g (setf *cursor* :garden))
    (:sdl-key-i (setf *cursor* :industrial))
    (:sdl-key-f (setf *cursor* :fire-department))
    (:sdl-key-p (setf *cursor* :police-department))))

(defun handle-mouse (x y)
  (when (sdl:mouse-left-p)
    (case *cursor*
      (:dozer
       (dozer x y)
       (play-sound :dozer))
      (t
       (build-tile x y *cursor*)))))

(defun cursor ()
  (let ((tile (snap-to-tile (sdl:mouse-x) (sdl:mouse-y))))
    (when tile
      (with-slots (x y) tile
	(let ((x (* x *tile-size*))
	      (y (* y *tile-size*))
	      (dimensions (sdl:cast-to-int (* (sqrt (or (sprite-data *cursor* 'dimensions) 1)) *tile-size*))))
;	  (sdl:draw-string-solid-* (format nil "(~A, ~A)" x y) x y)
	  (sdl:with-color (col *build-color*)
	    (sdl:draw-rectangle-* x y dimensions dimensions)))))))

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
      (setf *menu-surface* (sdl:create-surface *menu-width* *screen-height*))
      (init-buttons)
      (setup-sprites)
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
