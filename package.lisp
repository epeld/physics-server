
;; 
;; Dependencies
(quicklisp:quickload :cffi)


(defpackage :peldan.resources
  (:nicknames :resources)
  (:use :cl)
  (:export :defun-pointer-converters))

(defpackage :peldan.ode
  (:use :cl :cffi))

(defpackage :peldan.physics
  (:nicknames :physics)
  (:use :cl :ode))

(defpackage :peldan.low-level-ode
  (:nicknames :low-level :ode)
  (:export :geom-id :world-id :space-id :ode-real
           :contact :contact-geom :geoms
           :surface :geom :fdir)
  (:use :cl))

(defpackage :peldan.c-api-info
  (:nicknames :c-api-info)
  (:export :all-id-types :all-defuns :all-function-symbols)
  (:use :cl :cffi))
