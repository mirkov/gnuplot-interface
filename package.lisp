;;;; package.lisp

(defpackage #:gnuplot-interface
  (:use #:cl)
  (:import-from :alexandria
		:symbolicate)
  (:export
   ;; Many exported commands have a second name to prevent conflicts
   ;; with other packages.  Thus `command' can also be called with
   ;; `gnuplot-command'
   ;;
   ;; In the lines below, if a line has two symbols, the second symbol
   ;; is alternate command name
   :start-gnuplot
   :init-gnuplot
   :hello-world :gnuplot-hello-world
   :test :gnuplot-test
   :reset :gnuplot-reset 
   :stop-gnuplot
   :command :gnuplot-command
   :echo-command :gnuplot-echo-command
	   
   :send-line :send-line-to-gnuplot
   :send-line-break :send-line-break-to-gnuplot)
  (:documentation
"Package for starting a gnuplot subprocess and sending commands to
it"))