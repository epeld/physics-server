
(in-package :low-level)

(cffi:define-foreign-library libode
  (:darwin (:or "libode.3.dylib" "libode.dylib"))
  (:unix (:or "libode.so.3" "libode.so"))
  (t (:default "libode")))
   
(cffi:use-foreign-library libode)

;; These types need to be supported:
(quote ( "dContactGeom" "dJointType" "dMass" "dContact" "dJointFeedback"))


;; Define a translation that removes the prefixed "d" and keeps "ID" intact 
(defmethod cffi:translate-name-from-foreign ((spec string)
                                             (package (eql *package*))
                                             &optional varp)
  (assert (< 1 (length spec)))
  (assert (eql (char spec 0) #\d))

  (if (string= "dReal" spec)
      'ode-real
      
      ;; Use subseq to remove the prefixed "d"
      (let ((name (cffi:translate-camelcase-name (subseq spec 1)
                                                 :special-words '("ID"))))
        (if varp (intern (format nil "*~a*" name)) name))))


;;
;;  Setup the standard data type for ODE: ode-real
;; 
(cffi:defcfun "dGetConfiguration" :string)

(defun is-single-precision-p ()
  (search "single_precision" (get-configuration)))

;; Determine precision
(if (is-single-precision-p)
    (cffi:defctype ode-real :float)
    (cffi:defctype ode-real :double))

;; 
;; IDs:
;; ("dSpaceID" "dGeomID" "dWorldID" "dJointGroupID" "dJointID" "dBodyID")
;; 

(defmacro def-id-types ()
  `(progn ,@(loop for type in (c-api-info:all-id-types)
               collect `(cffi:defctype ,(cffi:translate-name-from-foreign type *package*) :pointer))))

(def-id-types)


;;
;;  Callbacks
;;
;; ("dNearCallback" "dColliderFn" "dMessageFunction")

;; Examples of how you can define your own:
(quote (cffi:defcallback near :void
           ((data :pointer)
            (o1 geom-id)
            (o2 geom-id))
         ))

(quote (cffi:defcallback collider :int
           ((o1 geom-id)
            (o2 geom-id)
            (flags :int)
            (contact (:pointer contact-geom))
            (skip :int))
         0))



(quote (cffi:defcallback msg-fn :void
           ((err-num :int)
            (msg :string))))


;;
;; Math stuff
;;
;;  "dMatrix3" "dQuaternion" "dVector3" etc
(cffi:defcstruct vector-3 
  (x ode-real)
  (y ode-real)
  (z ode-real))


(cffi:defcstruct vector-4 
  (x ode-real)
  (y ode-real)
  (z ode-real)
  (q ode-real))


(cffi:defcstruct matrix-3 
  (value ode-real :count 12))           ;4 x 3 or something..


(cffi:defcstruct matrix-4
  (value ode-real :count 16))


(cffi:defcstruct matrix-6
  (value ode-real :count 48))


(cffi:defcstruct quaternion
  (value ode-real :count 4))
