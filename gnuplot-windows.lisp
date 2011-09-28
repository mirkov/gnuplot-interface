;; Mirko Vukovic
;; Time-stamp: <2011-09-28 09:49:49EDT gnuplot-windows.lisp>
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

(in-package :gnuplot-interface)

;;;; Managing multiple windows.  

(defun window-equal (window1 window2)
  "Return t if two window descriptors are equal

The comparison is performed on the ID and the stream"
  (destructuring-bind (id-1 (process-1 stream-1)) window1
    (declare (ignore process-1))
    (destructuring-bind (id-2 (process-2 stream-2)) window2
      (declare (ignore process-2))
      (and (equal id-1 id-2)
	   (equal stream-1 stream-2)))))


(defun new-window (&optional (name "CL-Gnuplot" name-p))
  "Create new window and store it in the *windows* list
Return the window descriptor (id (process-id command-stream))"
  (let ((window-info (multiple-value-list (start-gnuplot))))
    (when window-info
      (let ((window-descriptor
	     (list (if name-p name
		       (format nil "~a-~a" name (length *windows*)))
		   window-info)))
	(push window-descriptor *windows*)
	(push window-descriptor *windows-history*)
	(init-gnuplot)
	(gnuplot-hello-world)
	window-descriptor))))

(defun list-windows ()
  (dolist (window *windows*)
    (format t "GNUplot window ~a on ~{process ~a and stream ~a~}~%"
	    (first window) (second window))))

(defun set-window (window-id)
"Set window to `window-id'.  `window-id' can be one of three:
 - positive index or 0, index of position on *windows* list
 - negative index, the i-th last window on the windows history
 - string, the window with the same name
 - A list of format returned by `new-window'

Return window info in same format as `new-window'

Takes care not to put the same window on top of windows history twice"
  (destructuring-bind (id (process stream))
      (cond
	((symbolp window-id)
	 (assoc window-id *windows*))
	((stringp window-id)
	 (assoc window-id *windows* :test #'string=))
	((and (integerp window-id)
	      (> 0 window-id))
	 (nth (- window-id) *windows-history*))
	((integerp window-id)
	 (nth window-id *windows*))
	((consp window-id)
	 window-id)
	(t
	 (error "Unsupported id type ~a.  Only symbols, integers,
	 strings and window-id lists are supported"
		(type-of window-id))))
    (unless (and process stream)
      (error "Window ID ~a does not match any of the windows" window-id))
    (unless (equal *command* stream)
      ;; change window only if we are choosing a different one
      (setf *gnuplot* process
	    *command* stream)

      (let ((window-id (list id (list *gnuplot* *command*))))
	(unless (window-equal window-id (first *windows-history*))
	    (push window-id *windows-history*))
	window-id))))



(defun kill-window ()
  "Terminate current *gnuplot* process, close current *command*
stream, remove the window info from *windows* and *windows-history*,
and activate last most recent window in history."
  (when *gnuplot*
    (format *command* "quit~%")
    (finish-output *command*)
    (destructuring-bind (input command-copy)
	(broadcast-stream-streams *command*)
      (close input)
      (close command-copy))
    (close *command*)
    #+clisp (progn
	      ;; (close *io*) (probably not necessary)
	      (close *input*)
	      (close *output*)
	      (close *command-copy*)
	      (setf *command-copy*
		    (make-string-output-stream)))
    #+sbcl(sb-ext:process-wait *gnuplot*)
    ;; make these nil before re-assigning them.  So, if there are no
    ;; more windows (windows list empty, history empty), they are not
    ;; pointing to non-existent windows
    (setf *gnuplot* nil
	  *command* nil)
    (let ((window-id (pop *windows-history*)))
      (setf *windows*
	    (remove window-id *windows* :test #'window-equal)
	    *windows-history*
	    (remove window-id *windows-history* :test #'window-equal))
      (when *windows-history*
	(set-window (first *windows-history*))))))