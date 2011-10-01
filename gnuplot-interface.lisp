;; Mirko Vukovic
;; Time-stamp: <2011-09-30 21:58:14 gnuplot-interface.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:gnuplot-interface)

(defvar *gnuplot* "Value returned by the command that invokes
gnuplot")

;; streams returned by the external processes
(defvar *input* nil "gnuplot stream")
(defvar *output* nil "gnuplot stream")
(defvar *io* nil "gnuplot bidirectional stream")
(defvar *error* nil "gnuplot stream")
(defvar *windows* nil "a-list of gnuplot streams and names")
(defvar *windows-history* nil "list of most recently used windows")
;; my streams
(defvar *command-copy* (make-string-output-stream)
  "Receives a copy of gnuplot commands")
(defvar *command* nil "Stream used to send commands")


(defvar *executable* #+sbcl "/usr/local/bin/gnuplot" #+clisp "gnuplot"
  "Path to the executable")
(defparameter *terminal* #+cysshd1 'wxt #-cysshd1 'x11)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+clisp (when (string= "" (sys::getenv "DISPLAY"))
	    (sys::setenv "DISPLAY" "127.0.0.1:0.0"))
  #+sbcl (unless (sb-ext:posix-getenv "DISPLAY")
	   (warn "DISPLAY is not set.  X11 terminal will not be operational" ))
  )



(defun start-gnuplot ()
  "Start gnuplot executable and initialize input stream.  Also create
the *command* broadcast stream

Return multiple values the *gnuplot* executable and the *command*
stream"
  (setf *command-copy* (make-string-output-stream))
  #+sbcl
  (setf *gnuplot* (sb-ext:run-program *executable*  nil
				      :wait nil
				      :input :stream
				      :output t
				      :error :output)
	*input* (sb-ext:process-input *gnuplot*)
	*command* (make-broadcast-stream *input* *command-copy*))
  #+(or (and clisp linux)
	(and clisp cygwin))
  (setf *gnuplot*
	  (multiple-value-setq (*io* *input* *output*)
	    (ext:make-pipe-io-stream *executable*
				     :buffered t))
	  *command*
	  (make-broadcast-stream *output* *command-copy*))
  (values *gnuplot* *command*))

(defun command (&rest command-and-args)
  "Pass `command-and-args' to the *command* stream"
;;  (let ((dump
;; clear output-stream
  (get-output-stream-string *command-copy*)
;;    (declare (ignore dump)))
  (when command-and-args
      (apply #'format  *command* command-and-args))
  (format *command* "~%")
  (finish-output *command*))

(define-symbol-macro gnuplot-command command)

(defun echo-command ()
  "Return the last command sent to " 
  (get-output-stream-string *command-copy*))

(define-symbol-macro gnuplot-echo-command echo-command)
(defun init-gnuplot ()
  (command "set terminal ~a" (alexandria:symbolicate *terminal*)))

(defun hello-world ()
  "Throw test plot"
  (command "plot cos(x)"))

(define-symbol-macro gnuplot-hello-world hello-world)



(defun clean-process-info ()
  "Set all special vars to nil"
  (setf *gnuplot* nil
	*input* nil
	*output* nil
	*io* nil
	*windows* nil
	*windows-history* nil
	*command-copy* nil
	*command* nil ))


(defun test (&key (terminal t) (palette :rgb))
  "Invoke gnuplot `test' command"
  (when terminal
    (command "test terminal")
    (return-from test))
  (when palette
    (command (format nil "test palette ~a" palette))
    (return-from test))
  (command "test"))

(define-symbol-macro gnuplot-test test)

(defun reset ()
  (command "reset"))
(define-symbol-macro gnuplot-reset reset)



