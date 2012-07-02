;; Mirko Vukovic
;; Time-stamp: <2012-06-21 16:48:56 gnuplot-interface.lisp>
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
(defvar *input* nil "input from the gnuplot stream to lisp")
(defvar *output* nil "lisp output to the gnuplot stream")
(defvar *io* nil "gnuplot bidirectional stream")
(defvar *error* nil "gnuplot error stream") ;; not used
(defvar *command-copy* (make-string-output-stream)
  "Receives a copy of gnuplot commands")
(defvar *command* nil "Stream used to send commands")

;;; Variables for multiple windows.  Currently not used
(defvar *windows* nil "a-list of gnuplot streams and names")
(defvar *windows-history* nil "list of most recently used windows")
;; my streams


(defparameter *executable*
  #+(and unix
	 (not (and clisp wgnuplot)))
  (or (probe-file "/usr/local/bin/gnuplot")
      "/usr/bin/gnuplot")
  #+(and clisp (not wgnuplot)) "gnuplot"
  #+(and clisp wgnuplot)
  (getf (symbol-plist :wgnuplot) :executable)
  "Path to the executable")

(defparameter *terminal*
  #+(and cygwin (not wgnuplot)) 'x11
  #+(and x11 (not cygwin)) 'wxt
  #+(and clisp wgnuplot) 'wxt
  #+(and unix
	 (not (and clisp wgnuplot))) 'x11
  "Default gnuplot terminal")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and cygwin (not wgnuplot)) (when (string= "" (sys::getenv "DISPLAY"))
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
  ;; we use external-program:run instead of external-program:start.
  ;;
  ;; This SBCL chunk of code is necessary because external-program:run
  ;; does not work.  I may revist this at a later date
  #+sbcl
  (setf *gnuplot* (sb-ext:run-program *executable*  nil
  				      :wait nil
  				      :input :stream
  				      :output t
  				      :error :output)
  	*output* (sb-ext:process-input *gnuplot*)
  	*input* (sb-ext:process-output *gnuplot*))
  #+(or (and clisp linux)
  	(and clisp cygwin))
  ;; we use external-program:run instead of external-program:start.
  (multiple-value-bind
	(status result)
      (external-program:run *executable* nil
			    :input :stream
			    :output :stream)
    (declare (ignore status))
    (setf *output* (two-way-stream-output-stream result)
	  *input* (two-way-stream-input-stream result)))
  ;; Historical note: In prior versions, on clisp I used
  ;; ext:run-program.  I had to set :wait to nil in order for the
  ;; command to work.  I moved to the external-program package, hoping
  ;; to be more universal.  But as of 2012-05-31, its code did not
  ;; work with SBCL.  It would just hang
  #|(multiple-value-setq (*io* *input* *output*)
  (ext:run-program *executable*
  :input :stream
  :output :stream
  :wait nil))|#
  (setf *command*
	(make-broadcast-stream *output* *command-copy*))
  (values))


(defun stop-gnuplot ()
  "Stop gnuplot and close all the streams"
  (command "quit")
  #+sbcl
  (progn
    (close *input*)
    (close *output*)
    (close *command-copy*)
    (close *command*))
  #+clisp
  (progn
    (close *input*)
    (close *output*)
    ;;(close *io*)
    (close *command*)
    (close *command-copy*)))

(defun stop ()
  "Alias for STOP-GNUPLOT"
  (stop-gnuplot))

(defun command (&rest command-and-args)
  "Pass `command-and-args' to the *command* stream"
#|  (get-output-stream-string *command-copy*)|#
  (when command-and-args
    (apply #'format  *command* command-and-args)
    (format *command* "~%")
    (finish-output *command*)))

(defun gnuplot-command (&rest args)
  "Alias for COMMAND"
  (apply #'command args))

(defun send-line (string)
  "Pass a single line to the gnuplot stream

This command is used to pass complex, multi-line input to gnuplot.
For it to take effect, the sequence of SEND-LINE commands must be
finished by the SEND-LINE-BREAK command."
  (princ string *command*)
  (finish-output *command*))

(defun send-line-break ()
  "Send a line break"
  (princ (format nil "~%") *command*)
  (finish-output *command*))
(defun send-line-to-gnuplot (string)
  (send-line string))
(defun send-line-break-to-gnuplot ()
  (send-line-break))


(defun echo-command ()
  "Return the last command sent to " 
  (get-output-stream-string *command-copy*))

(defun gnuplot-echo-command ()
  (echo-command))

(defun init-gnuplot ()
  (command "set terminal ~a" (string-downcase *terminal*) #|(alexandria:symbolicate *terminal*)|#))

(defun hello-world ()
  "Throw test plot"
  (command "plot cos(x)"))

(defun gnuplot-hello-world ()
  (hello-world))



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

(defun gnuplot-test (&rest keywords &key &allow-other-keys)
  (apply #'test :allow-other-keys t keywords))

(defun reset ()
  (command "reset"))
(defun gnuplot-reset ()
  (reset))



