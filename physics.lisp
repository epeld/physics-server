
(in-package :physics)

(defclass simulation ()
  ((world :accessor simulation-world :documentation "ODE WorldID handle")
   (step-size :accessor simulation-step-size :type float)
   (contacts :accessor contact-group :documentation "Collision contacts between objects")))


(defun collision-check (simulation)
  "Perform collision checking, adding joints as needed"
  (clear-contacts simulation)
  (ode:space-collide (simulation-space simulation) simulation #'near-callback))


;; This is the callback when two objects are potentially colliding
;; Transcribed from ODE demo projects
(cffi:defcallback near-callback :void
    ((simulation :pointer)
     (object1 geom-id)
     (object2 geom-id))
  
  "Generate contacts between object1 and object2"

  (the simulation simulation)
  (assert (not (null (contact-group simulation))))
  
  (assert object1)
  (assert object2)

  (if (or (geom-is-space object1)
          (geom-is-space object1))
      (space-collide-2 object1 object2 simulation (cffi:callback near-callback))

      (cffi:with-foreign-object (contact '(:struct ode:contact) 32)
        (let ((n (ode:collide object1 object2
                              32
                              (cffi:foreign-slot-pointer contact '(:struct contact) 'contact-geom)
                              (cffi:foreign-type-size '(:struct contact)))))
          (unless (zerop n)
            (loop for index from 0 upto (- n 1) do

                 (let* ((contact (cffi:mem-aref contact '(:struct contact) index))
                        (geom (cffi:foreign-slot-value contact '(:struct contact) 'contact-geom))
                        (geoms (cffi:foreign-slot-value geom '(:struct contact-geom) 'geoms))

                        ;; TODO go ahead and adjust the contact params here later
                        (joint (joint-create-contact (simulation-world simulation)
                                                     (contact-group simulation)
                                                     (cffi:mem-aptr contact '(:struct contact)))))
                   
                   (joint-attach joint
                                 (cffi:mem-aref geoms 'geom-id 0)
                                 (cffi:mem-aref geoms 'geom-id 1)))))))))


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
