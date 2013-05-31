;;;; simcity.asd
(defpackage #:simcity-config (:export #:*base-directory*))
(defparameter simcity-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))


(asdf:defsystem #:simcity
  :serial t
  :description "Describe simcity here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
	       #:lispbuilder-sdl-mixer)
  :components ((:file "package")
               (:file "simcity")
	       (:file "render")))

