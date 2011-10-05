;;;; gnuplot-interface.asd

(asdf:defsystem #:gnuplot-interface
  :serial t
  :components ((:file "package")
               (:file "gnuplot-interface")
	       (:file "gnuplot-windows")
(:file "examples"))
  :depends-on (:alexandria))

