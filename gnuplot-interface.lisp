;; Mirko Vukovic
;; Time-stamp: <2012-11-21 09:03:12EST gnuplot-interface.lisp>
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



(defparameter *gnuplot* "Value returned by the command that invokes
gnuplot")

;; streams returned by the external processes
(defparameter *gnuplot-output* nil "input from the gnuplot stream to lisp")
(defparameter *gnuplot-input* nil "lisp output to the gnuplot stream")
(defparameter *io* nil "gnuplot bidirectional stream")
(defparameter *error* nil "gnuplot error stream") ;; not used
(defparameter *command-copy* (make-string-output-stream)
  "Receives a copy of gnuplot commands")
(defparameter *command* nil "Stream used to send commands")

;;; Variables for multiple windows.  Currently not used
(defparameter *windows* nil "a-list of gnuplot streams and names")
(defparameter *windows-history* nil "list of most recently used windows")
;; my streams
(defparameter *native-external-program* t
  "Controls compilation of the START-GNUPLOT command

If T, we use SBCL and CLISP native commands for starting the gnuplot
process.  If NIL we use the EXTERNAL-PROGRAM package to start the
process.

Currently, we use native commands as I could not figure out a uniform
way of starting gnuplot across platforms.  See code documentation in
START-GNUPLOT")

(defparameter *executable*
  #+darwin
  (probe-file "/opt/local/bin/gnuplot")
  #+(and unix
	 (not darwin)
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
the *command* broadcast stream.
"
  (setf *command-copy* (make-string-output-stream))

;;; I currently use native facilities for starting the gnuplot
;;; process.  Note that in SBCL I set :WAIT to NIL and in CLISP I set
;;; :WAIT to T.  Otherwise the processes will hang or consume 100% of
;;; CPU.
  #+(and native-external-program
	 sbcl)
  (setf
   *gnuplot*
   (sb-ext:run-program
    *executable* nil ;;'("-persist" "-e" "plot sin(x)")
    :wait nil
    :input :stream
    :output :stream
    :error :output)
   *gnuplot-input* (sb-ext:process-input *gnuplot*)
   *gnuplot-output* (sb-ext:process-output *gnuplot*))
  #+(and native-external-program
	 (and clisp cygwin))
  (multiple-value-setq (*io* *gnuplot-output* *gnuplot-input*)
    (ext:run-program *executable*
		     :arguments nil ;;'("-p" "-e" "plot sin(x)")
		     :input :stream
		     :output :stream
		     :wait t))

;;; The following does not work uniformly across SBCL and CLISP.  I
;;; leave it here as documentation to use when I learn what is causing
;;; the issue between SBCL and CLISP.
  #+external-program
  (setf *gnuplot* (external-program:start *executable* nil ;;'("-p" "-e" "plot sin(x)")
					  :input :stream
					  :output :stream)
	*gnuplot-input* (external-program:process-input-stream *gnuplot*)
  	*gnuplot-output* (external-program:process-output-stream *gnuplot*))
  (setf *command*
	(make-broadcast-stream *gnuplot-input* *command-copy*))
  (values))


(defun stop-gnuplot ()
  "Stop gnuplot and close all the streams"
  (unwind-protect
  (command "quit")
    #+sbcl
    (progn
    ;;(close *gnuplot-output*)
    (close *gnuplot-input*)
    (close *command-copy*)
    (close *command*))
    #+clisp
    (progn
      (close *gnuplot-output*)
      (close *gnuplot-input*)
      ;;(close *io*)
      (close *command*)
      (close *command-copy*))))

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
	*gnuplot-output* nil
	*gnuplot-input* nil
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



