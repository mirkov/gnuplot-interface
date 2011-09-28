;;;; package.lisp

(defpackage #:gnuplot-interface
  (:use #:cl)
  (:import-from :alexandria
		:symbolicate)
  (:export :start-gnuplot :init-gnuplot :stop-gnuplot
	    :new-window :list-windows :set-window :kill-window
	    :gnuplot-command :gnuplot-echo-command
	    :gnuplot-hello-world :gnuplot-test :gnuplot-reset))