
(in-package :physics)

;; (make-world :hash-space)
;; (make-space)


;; Our objects are combinations of geoms and bodyIDs
;; Except for independent boxes (static ones)
;; (make-box)                              
;; (make-sphere)
;; (add-object world box)

(defstruct vector3d x y z)

;; TODO maybe we don't need these, maybe we can just use geom-ids directly
;; and call dGeomGetBody to get the body-id instead
(defclass object ()
  ((body-id :accessor body-id :documentation "ODE BodyID handle")
   (geom-id :accessor geom-id :documentation "ODE GeomID handle"))
  (:documentation "A Physical Object that can interact with the rest of the simulation"))



(defclass settings ()
  ((step-size :accessor step-size :type :float))
  (:documentation "Simulation settings"))

(defclass simulation ()
  ((world-id :accessor world-id :documentaiton "ODE WorldID handle")
   (space-id :accessor space-id :documentation "ODE SpaceID handle")
   (objects :accessor simulation-objects :documentation "A list of objects")
   (settings :accessor simulation-settings :type settings)))


(defun collide (simulation near-fn)
  "Find collisions of objects inside the world, calling near-fn with those pairs of objects"
  :todo)

(defun contacts (object1 object2 &optional (count 16))
  "Generate contacts between object1 and object2"
  :todo)

(defun step (simulation)
  "Step the simulation forward"
  :todo)




