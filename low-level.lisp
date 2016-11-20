
(in-package :low-level)

(cffi:define-foreign-library libode
  (:darwin (:or "libode.3.dylib" "libode.dylib"))
  (:unix (:or "libode.so.3" "libode.so"))
  (t (:default "libode")))
   
(cffi:use-foreign-library libode)


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
                                                 :special-words '("ID" "ERP" "CFM" "ODE" "2D" "PR" "PU"))))
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

(cffi:defctype near-callback :pointer)
(cffi:defctype collider-fn :pointer)
(cffi:defctype message-function :pointer)

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

;;
;; Joint Stuff
;;
(cffi:defcstruct surface-parameters
  (mode :int)
  (mu ode-real :count 2)
  (rho ode-real :count 3)
  (bounce ode-real :count 2)
  
  (soft-erp ode-real)
  (soft-cfm ode-real)
  
  (motion ode-real :count 3)
  (slip ode-real :count 2))


(cffi:defcstruct contact-geom
  (position (:struct vector-3))
  (normal (:struct vector-3))
  (depth ode-real)
  (geoms geom-id :count 2)
  (sides :int :count 2))


(cffi:defcstruct contact 
  (surface (:struct surface-parameters))
  (geom (:struct contact-geom))
  (fdir (:struct vector-3)))


;;
;;  Joint Types
;; 

(defmacro def-joint-types ()
  (let ((types '("dJointTypeNone"
                 "dJointTypeBall"
                 "dJointTypeHinge"
                 "dJointTypeSlider"
                 "dJointTypeContact"
                 "dJointTypeUniversal"
                 "dJointTypeHinge2"
                 "dJointTypeFixed"
                 "dJointTypeNull"
                 "dJointTypeAMotor"
                 "dJointTypeLMotor"
                 "dJointTypePlane2D"
                 "dJointTypePR"
                 "dJointTypePU"
                 "dJointTypePiston"
                 "dJointTypeDBall"
                 "dJointTypeDHinge"
                 "dJointTypeTransmission")))
    `(cffi:defcenum joint-type
       ,@ (mapcar (lambda (x)
                    (intern (string (cffi:translate-name-from-foreign x *package*)) (find-package :keyword)))
                  types))))

(def-joint-types)

;;
;;  Mass
;;

;; struct dMass {
;;   dReal mass;
;;   dVector3 c;
;;   dMatrix3 I;

(cffi:defcstruct mass
  (mass ode-real)
  (c (:struct vector-3))
  (i (:struct matrix-3)))


;;
;;  Joint feedback
;;

;; typedef struct dJointFeedback {
;;   dVector3 f1;		/* force applied to body 1 */
;;   dVector3 t1;		/* torque applied to body 1 */
;;   dVector3 f2;		/* force applied to body 2 */
;;   dVector3 t2;		/* torque applied to body 2 */
;; } dJointFeedback;

(cffi:defcstruct joint-feedback
  (f1 (:struct vector-3))
  (t1 (:struct vector-3))
  (f2 (:struct vector-3))
  (t2 (:struct vector-3)))


;;
;; Definition of the API
;;

(cffi:defctype :file :pointer)

(defmacro define-api ()
  `(progn ,@(c-api-info:all-defuns)))

(define-api)

;; Export API
(export (c-api-info:all-function-symbols))
