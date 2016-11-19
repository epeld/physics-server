
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

(defpackage :peldan.low-level-ode
  (:nicknames :low-level)
  (:use :cl))

(defpackage :peldan.c-api-info
  (:nicknames :c-api-info)
  (:export :all-id-types)
  (:use :cl :cffi))
