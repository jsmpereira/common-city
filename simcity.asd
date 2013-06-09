;;;; simcity.asd
(defpackage #:simcity-config (:export #:*base-directory*))
(defparameter simcity-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))


(asdf:defsystem #:simcity
  :serial t
  :description "(Very early) Beginnings of a Simcity clone. Gamedev Sundays."
  :author "Jose Santos Martins Pereira <jsmpereira@gmail.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
	       #:lispbuilder-sdl-image
	       #:lispbuilder-sdl-mixer)
  :components ((:file "package")
               (:file "simcity")
	       (:file "render")))

