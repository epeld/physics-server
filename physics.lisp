
(in-package :physics)

;; (make-world :hash-space)
;; (make-space)


;; Our objects are combinations of geoms and bodyIDs
;; Except for independent boxes (static ones)
;; (make-box)                              
;; (make-sphere)
;; (add-object world box)

(defstruct vector3d x y z)

;; TODO consider adding a joint group id for contact joints
(defclass simulation ()
  ((world :accessor simulation-world :documentation "ODE WorldID handle")
   (space :accessor simulation-space :documentation "ODE SpaceID handle")
   (objects :accessor simulation-objects :documentation "A list of objects (geoms)")
   (step-size :accessor simulation-step-size :type float)))


(defun collision-check (simulation)
  "Perform collision checking, adding joints as needed"
  (ode:space-collide (simulation-space simulation) simulation #'near-callback))

;; this is the callback when two objects are potentially colliding
(defun near-callback (simulation object1 object2)
  "Generate contacts between object1 and object2"
  :todo)

(defun step (simulation)
  "Step the simulation forward"
  :todo)


;; TODO define object creation functions:
;; (make-box)
;; (make-sphere)
;; etc



