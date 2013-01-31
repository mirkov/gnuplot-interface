;; Mirko Vukovic
;; Time-stamp: <2013-01-31 13:54:25Eastern Standard Time examples.lisp>
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


(defun plot-sin ()
  (send-line "plot sin(x)")
  (send-line (format nil "~%"))
  (finish-output *command*))

(defun plot-inline-simple ()
  "Example of a plot with inlined data.  This example also illustrates
the use of sending a line break"
  (send-line "plot '-' with lines")
  (send-line "5")
  (send-line "12")
  (send-line "-8")
  (send-line "e")
  (finish-output *command*))

(defun plot-fun (numbers function)
  "Given a list of numbers `numbers' and a `function', generate a
plot

This example shows how to make a plot of x vs y using the inlined data"
  (send-line "plot '-' using 1:2 with lines")
  (dolist (x numbers)
    (send-line (format nil "~a ~a" x (funcall function x))))
  (send-line #\e)
  (finish-output *command*))

(defun plot-combo (numbers function)
  "This example builds upon `plot-fun'.  Along with the function plot, it overplots gnuplot's cos(x)"
  (send-line "plot '-' using 1:2 with lines" t)
  (send-line ", cos(x)")
  (dolist (x numbers)
    (send-line (format nil "~a ~a" x (funcall function x))))
  (send-line #\e)
  (finish-output *command*))
