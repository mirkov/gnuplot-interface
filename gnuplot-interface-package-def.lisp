;;;; package.lisp

(defpackage #:gnuplot-interface
  (:nicknames #:gpi)
  (:use #:cl)
  #+skip-external-program(:use #:external-program)
  (:import-from :alexandria
		:symbolicate)
  (:export
   ;; Many exported commands have a second name to prevent conflicts
   ;; with other packages.  Thus `command' can also be called with
   ;; `gnuplot-command'
   ;;
   ;; In the lines below, if a line has two symbols, the second symbol
   ;; is alternate command name
   :start :start-gnuplot
   :init-gnuplot
   :hello-world :gnuplot-hello-world
   :test :gnuplot-test
   :reset :gnuplot-reset 
   :stop :stop-gnuplot
   :command :gnuplot-command
   :echo-command :gnuplot-echo-command
	   
   :send-line :send-line-to-gnuplot
   :send-line-break :send-line-break-to-gnuplot

   :*terminal*)
  (:documentation
"Package for starting a gnuplot subprocess and sending gnuplot
commands to it.  This package does not introduce plotting commands of
its own.  It only serves to send command strings to the gnuplot
process.

The package symbols are not meant to be imported into user packaes. 
They are  intended to be used as:
gpi:start-gnuplot
gp1:command
gpi:test
gpi:...

"))


(unless (find :native-external-program *features*)
  (push :native-external-program *features*))