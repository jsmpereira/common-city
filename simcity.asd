;;;; simcity.asd

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

