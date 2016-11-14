
(in-package :physics)

(defstruct vector3d x y z)

(defclass simulation ()
  ((world :accessor simulation-world :documentation "ODE WorldID handle")
   (space :accessor simulation-space :documentation "ODE SpaceID handle")
   (objects :accessor simulation-objects :documentation "A list of objects (geoms)")
   (step-size :accessor simulation-step-size :type float)
   (contacts :accessor simulation-contacts :documentation "Collision contacts between objects")))


(defun collision-check (simulation)
  "Perform collision checking, adding joints as needed"
  (clear-contacts simulation)
  (ode:space-collide (simulation-space simulation) simulation #'near-callback))

;; this is the callback when two objects are potentially colliding
(defun near-callback (simulation object1 object2)
  "Generate contacts between object1 and object2"
  ;; TODO add contacts to simulation
  :todo)

(defun step (simulation)
  "Step the simulation forward"
  :todo)


(defun simulate (simulation)
  (collision-check simulation)
  (step simulation))

;; TODO define object creation functions:
;; (make-box)
;; (make-sphere)
;; etc



