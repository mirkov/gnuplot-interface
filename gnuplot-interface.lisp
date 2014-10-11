;; Mirko Vukovic
;; Time-stamp: <2014-10-09 11:26:03Eastern Daylight Time gnuplot-interface.lisp>
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



(defparameter *gnuplot* nil
  "The gnuplot process")

;; streams returned by the external processes
(defparameter *gnuplot-output* nil "input from the gnuplot stream to lisp")
(defparameter *gnuplot-input* nil "lisp output to the gnuplot stream")
(defparameter *io* nil "gnuplot bidirectional stream")
(defparameter *error* nil "gnuplot error stream") ;; not used
#+skip(defparameter *command-copy* (make-string-output-stream)
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun executable-path ()
    "Return path to the executable"
    (let ((candidates
	   (list
	    ;; Darwin
	    #+darwin
	    (probe-file "/opt/local/bin/gnuplot")
	    ;; Pure unix (not cygwin using windows gnuplot)
	    #+(and unix
		   (not darwin)
		   (not wgnuplot))
	    (or (probe-file "/usr/local/bin/gnuplot")
		(probe-file "/usr/bin/gnuplot"))
	    ;; cygwin and windows gnuplot (wgnuplot)
	    #+wgnuplot ;; implies windows
	    (getf (symbol-plist user-setup::*gnuplot*) :executable)
	    )))
    (let ((path (case (length candidates)
		  (0 (error "No paths to GNUPLOT executable - check gnuplot-interface.lisp"))
		  (1 (car candidates))
		  (t (error "Found more than one candidate path: ~a 
Check gnuplot-interface.lisp" candidates)))))
      (assert (stringp path) ()
	      "Path ~a must be a string or path" path)
      (assert (probe-file path) ()
	      "Executable ~a does exist" path)
      path))))

(defparameter *executable* (executable-path)
  "Path to the executable")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun default-terminal ()
    (let ((term-candidates
	   (list
	    #+darwin nil
	    #+(and x11 (not cygwin)) 'wxt
	    ;; cygwin+clisp+wgnuplot supports WXT
	    #+(and cygwin clisp wgnuplot) 'wxt
	    ;; CCL+wgnuplot does not support WXT only WINDOWS
	    #+(and ccl wgnuplot) 'windows
	    )))
      (let ((term (case (length term-candidates)
		    (0 (error "No default terminal selected - check gnuplot-interface.lisp"))
		    (1 (car term-candidates))
		    (t (error "Found more than one terminal candidate: ~a 
Check gnuplot-interface.lisp" term-candidates)))))
	(assert term ()
		"Terminal is undefined (set to nil) -- edit source file to fix this")
	term))))

(defparameter *terminal* (default-terminal)
  "Default gnuplot terminal")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and (or unix darwin))
  (when (string= "" (uiop/os:getenv "DISPLAY"))
    #+sbcl (sb-ext:posix-setenv "DISPLAY" "127.0.0.1:0.0")
    #+clisp (setf (ext:getenv "DISPLAY") "127.0.0.1:0.0")
    #+ccl (ccl:setenv "DISPLAY") "127.0.0.1:0.0")
  )




(defun start-gnuplot ()
  "Start gnuplot executable and initialize input stream.  Also create
the *command* broadcast stream.
"
  (assert (uiop/os:getenv "DISPLAY") ()
	  "DISPLAY  environemnt variable not set.  Exiting
Check gnuplot-interface.lisp")
  #+skip(setf *command-copy* (make-string-output-stream))

;;; I currently use native facilities for starting the gnuplot
;;; process.  Note that in SBCL I set :WAIT to NIL and in CLISP I set
;;; :WAIT to T.  Otherwise the processes will hang or consume 100% of
;;; CPU.
  #+(and native-external-program windows ccl)
  (setf *gnuplot*
	(ccl:run-program ;;"cmd.exe"
			 ;;(list *executable*)
	 ;;(format nil "\"~a\"" *executable*)
	 *executable*
	 #+skip nil
	 '("-p" "-e" "plot sin(x)")
			 :wait nil
			 :input :stream
			 :output :stream
			 :error :output)
	*gnuplot-input* (ccl:external-process-input-stream *gnuplot*)
	*gnuplot-output* (ccl:external-process-output-stream *gnuplot*))
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
  (setf *command* *gnuplot-input*)
  (values))


(defun stop-gnuplot ()
  "Stop gnuplot and close all the streams"
  (unwind-protect
  (command "quit")
    #+ccl
    (progn
      )
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
      #+skip(close *command-copy*))))

(defun stop ()
  "Alias for STOP-GNUPLOT"
  (stop-gnuplot))

(defparameter *gnuplot-input-string* ""
  "Stores command string sent to gnuplot" )

(defmacro with-captured-gnuplot-input (&body body)
  "Execute body while capturing commands sent to *command* into 
*gnuplot-input-string*"
  `(let* ((copy-stream (make-string-output-stream))
	  (command-stream (make-broadcast-stream *gnuplot-input*
						 copy-stream)))
     (let ((*command* command-stream))
       (unwind-protect
	    ,@body
	 (setf *gnuplot-input-string* (get-output-stream-string copy-stream))
	 (close copy-stream)))))

(defun command (&rest command-and-args)
  "Pass `command-and-args' to the *command* stream"
  #+skip(get-output-stream-string *command-copy*)
  (when command-and-args
    (apply #'format  *command* command-and-args)
    (format *command* "~%")
    (finish-output *command*)))

(defun gnuplot-command (&rest args)
  "Alias for COMMAND"
  (apply #'command args))

(defun send-string (string)
  "Send a string to gnuplot

Do not send line return
Do not attempt to flush the buffer"
  (princ string *command*))

(defun send-line (string &optional continuation)
  "Pass a single line to the gnuplot stream

Send a line return

If CONTINUATION is T also append the \ character
Do not attempt to flush the buffer

This command is used to pass complex, multi-line input to gnuplot."
  (princ string *command*)
  (when continuation (princ #\\ *command*))
  (princ "
" *command*))



(defun send-line-break ()
  "Send a line break.

Do not attempt to flush the buffer"
  (princ (format nil "
") *command*))

(defun flush-buffer ()
  "Flush buffer"
  (finish-output *command*))

(defun finish-command ()
  "Send line-break and flush stream"
  (princ "
" *command*)
  (finish-output *command*))


(defun send-line-to-gnuplot (string)
  (send-line string))
(defun send-line-break-to-gnuplot ()
  (send-line-break))


#+skip(defun echo-command ()
  "Return the last command sent to " 
  (get-output-stream-string *command-copy*))

#+skip(defun gnuplot-echo-command ()
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
	;;*command-copy* nil
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


(defun gnuplot-for-windows-p ()
  "Return true if running gnuplot for windows"
  (search "Program Files" gpi::*executable*))

(defun normalize-namestring (path)
  "Normalize namestring to underlying OS

This function is used when running clisp on cygwin and Gnuplot for windows"
  #+cygwin
  (if (gnuplot-for-windows-p)
      (let ((stream
	     (ext:run-shell-command
	      (concatenate 'string
			   "cygpath -m "
			   (namestring path))
	      :output :stream)))
	(prog1
	    (read-line stream)
	  (close stream)))
      (namestring path))
  #-cygwin
  (namestring path))
