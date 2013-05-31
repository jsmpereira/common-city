;;; -*- Mode: Lisp -*-

(in-package :simcity)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *bg-color* sdl:*black*)

(defparameter *scale* 20)

(defparameter *cursor* :dozer)

(defparameter *assets-dir* 
  (merge-pathnames #P"assets/" simcity-config:*base-directory*))

(defparameter *audio-assets*
  `(:dozer ,(merge-pathnames "rumble.wav" *assets-dir*)
	   :bop ,(merge-pathnames "bop.wav" *assets-dir*)))

 (defun play-sound (asset)
   (let ((sound (sdl-mixer:load-sample (getf *audio-assets* asset))))
     (sdl-mixer:play-sample sound)))

(defun render-step ()
  (do-world (i j)
    (draw (aref *world* i j)))
  (loop for entity in *entities* do
	(draw-residencial entity)))

(defun render ()
  (render-step)
  (cursor)
  (sdl:update-display))

(defun handle-keys (key)
  (case key
    (:sdl-key-x (reset))
    (:sdl-key-d (setf *cursor* :dozer))
    (:sdl-key-r (setf *cursor* :residencial))
    (:sdl-key-t (setf *cursor* :road))))

(defun handle-mouse ()
  (when (and (sdl:mouse-left-p); (sdl:mouse-released-p button)
	     )
    (case *cursor*
      (:dozer
       (simple-tile (sdl:mouse-x) (sdl:mouse-y) :dirt)
       (play-sound :dozer))
      (:road
       (simple-tile (sdl:mouse-x) (sdl:mouse-y) :road))
      (:residencial
       (unless (residencial (sdl:mouse-x) (sdl:mouse-y))
	(play-sound :bop))))))

(defun cursor ()
  (let ((tile (snap-to-tile (sdl:mouse-x) (sdl:mouse-y))))
    (with-accessors ((tile-x x) (tile-y y) (size size)) tile
      (case *cursor*
	(:dozer
	 (sdl:draw-rectangle-* tile-x tile-y 20 20 :color sdl:*white*))
	(:residencial
	 (sdl:draw-rectangle-* tile-x tile-y  60 60 :color sdl:*blue*))
	(:road
	 (sdl:draw-rectangle-* tile-x tile-y 20 20 :color sdl:*black*))))))

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
      (reset)
      (sdl:with-events ()
	(:quit-event () (sdl-mixer:close-audio) t)
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
	       (render))))))

