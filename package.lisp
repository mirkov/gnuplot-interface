;;;; package.lisp

(defpackage #:gnuplot-interface
  (:use #:cl)
  (:import-from :alexandria
		:symbolicate)
  (:export :start-gnuplot :init-gnuplot :hello-world :stop-gnuplot
	   :command :echo-command :reset :test
	   :send-line :send-line-break
	   ;; alternate names, less likely to cause conflicts with
	   ;; other pakcages
	   :gnuplot-hello-world 
	   :gnuplot-command :gnuplot-echo-command
	   :gnuplot-reset :gnuplot-test
	   ))