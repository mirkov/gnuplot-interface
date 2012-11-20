;;;; gnuplot-interface.asd

(asdf:defsystem #:gnuplot-interface
  :serial t
  :components ((:file "gnuplot-interface-package-def")
               (:file "gnuplot-interface")
	       (:file "gnuplot-windows")
	       (:file "examples"))
  :depends-on (:alexandria
	       #+skip-external-program :external-program))

