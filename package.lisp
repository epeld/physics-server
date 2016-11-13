
;; 
;; Dependencies
(quicklisp:quickload :cffi)


(defpackage :peldan.resources
  (:nicknames :resources)
  (:use :cl)
  (:export :defun-pointer-converters))


(defpackage :peldan.ode
  (:nicknames :ode)
  (:use :cl :cffi))


(defpackage :peldan.physics
  (:nicknames :physics)
  (:use :cl))
