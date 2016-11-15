   
(in-package :ode)

(define-foreign-library libode
    (:darwin (:or "libode.3.dylib" "libode.dylib"))
    (:unix (:or "libode.so.3" "libode.so"))
    (t (:default "libode")))
   
(use-foreign-library libode)

(defmethod translate-name-from-foreign ((spec string)
                                        (package (eql *package*))
                                        &optional varp)
  (let ((name (translate-camelcase-name (subseq spec 1)
                                        :special-words '("ERP" "CFM" "ODE"))))
    (if varp (intern (format nil "*~a*" name)) name)))

;;
;; Info
(defcfun "dGetConfiguration" :string)

(defun is-single-precision-p ()
  (search "single_precision" (get-configuration)))

;; Determine precision
(if (is-single-precision-p)
    (defctype ode-real :float)
    (defctype ode-real :double))

;;typedef dReal dVector3[4];
;;typedef dReal dVector4[4];
;;typedef dReal dMatrix3[4*3];
;;typedef dReal dMatrix4[4*4];
;;typedef dReal dMatrix6[8*6];
;;typedef dReal dQuaternion[4];

(defcstruct vector3 
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcstruct matrix3
  (value ode-real :count 12))


;;
;; Initialization

(defbitfield init-flags
  (:none 0)
  :manual-cleanup)

(defbitfield allocate-flags
  (:basic 0)
  (:collision #x00000001)
  (:all #.(lognot 0)))


(defcfun "dInitODE2" :uint (flags init-flags))
(defcfun "dCloseODE" :void)

(defcfun "dAllocateODEDataForThread" :uint (flags allocate-flags))

;; Go ahead and initialize everything directly
;; (assumes we are main thread)
(init-ode-2 '(:none))
(allocate-ode-data-for-thread '(:all))

;;
;;  World API
;; 
(defctype world-id :pointer)
(defctype space-id :pointer)

;; Creation
(defcfun "dWorldCreate" world-id)
(defcfun "dWorldDestroy" :void (world-id world-id))

;; Gravity
(defcfun "dWorldSetGravity" :void
  (world-id world-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun "dWorldGetGravity" :void 
  (world-id world-id)
  (vector (:pointer (:struct vector3))))

;; ERP
(defcfun "dWorldSetERP" :void (world-id world-id) (erp ode-real))
(defcfun "dWorldGetERP" ode-real (world-id world-id))

;; CFM
(defcfun "dWorldSetCFM" :void (world-id world-id) (cfm ode-real))
(defcfun "dWorldGetCFM" ode-real (world-id world-id))

;; Update / Stepping
(defcfun "dWorldStep" :void (world-id world-id) (step-size ode-real))
(defcfun "dWorldQuickStep" :void (world-id world-id) (step-size ode-real))

;;
;; Body
;; 
(defctype body-id :pointer)

(defcfun "dBodyCreate" body-id
  (world-id world-id))

(defcfun "dBodyDestroy" body-id
  (body-id body-id))

;;
;; Geom
;; 
(defctype geom-id :pointer)

;; Creation
(defcfun "dCreateBox" geom-id
  (space-id space-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun "dCreateSphere" geom-id
  (space-id space-id)
  (radius ode-real))

(defcfun "dCreatePlane" geom-id
  (space-id space-id)
  (a ode-real)
  (b ode-real)
  (c ode-real)
  (d ode-real))

(defcfun "dGeomDestroy" :void
  (geom-id geom-id))


;; Bodies
(defcfun "dGeomGetBody" body-id
  (geom-id geom-id))

(defcfun "dGeomSetBody" :void
  (geom-id geom-id)
  (body-id body-id))

;; Position
(defcfun "dGeomSetPosition" :void
  (geom-id geom-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun "dGeomGetPosition" (:pointer ode-real)
  (geom-id geom-id))


;; Rotation
(defcfun "dGeomGetRotation" (:pointer (:struct matrix3))
  (geom-id geom-id))


(defcfun "dGeomSetRotation" :void
  (geom-id geom-id)
  (r (:pointer (:struct matrix3))))

;; 
;; Space
;;

(defcfun "dHashSpaceCreate" space-id (space-id space-id))

(defcfun "dSpaceAdd" :void (space-id space-id) (geom-id geom-id))
(defcfun "dSpaceRemove" :void (space-id space-id) (geom-id geom-id))
(defcfun "dSpaceClean" :void (space-id space-id))

(defcfun "dSpaceGetNumGeoms" :int (space-id space-id))
(defcfun "dSpaceGetGeom" geom-id (space-id space-id) (index :int))

;;
;; Collision
;;

(defcfun "dSpaceCollide" :void
  (space-id space-id)
  (data :pointer)
  (near-callback :pointer))

;; TODO figure out contact geom format
(defcfun "dCollide" :int
  (o1 geom-id)
  (o2 geom-id)
  (flags :int)
  (contact :pointer)
  (skip :int))

;;
;;  Rotation
;;

(defcfun "dRFromAxisAndAngle" :void
  (r (:pointer (:struct matrix3)))
  (ax ode-real)
  (ay ode-real)
  (bz ode-real)
  (angle ode-real))

(defcfun "dRSetIdentity" :void
  (r (:pointer (:struct matrix3))))

;;
;;   Contacts
;;

(defctype joint-id :pointer)
(defctype joint-group-id :pointer)


(defcstruct surface-parameters
  (mode :int)
  (mu ode-real :count 2)
  (rho ode-real :count 3)
  (bounce ode-real :count 2)
  
  (soft-erp ode-real)
  (soft-cfm ode-real)
  
  (motion ode-real :count 3)
  (slip ode-real :count 2))


(defcstruct contact-geom
  (position (:struct vector3))
  (normal (:struct vector3))
  (depth ode-real)
  (geoms geom-id :count 2)
  (sides :int :count 2))


(defcstruct contact 
  (surface (:struct surface-parameters))
  (contact-geom (:struct contact-geom))
  (fdir (:struct vector3)))


(defcfun "dJointCreateContact" joint-id
  (world-id world-id)
  (joint-group-id joint-group-id)
  (contact (:pointer (:struct contact))))

;;
;;  Joint Groups
;; 
(defcfun "dJointGroupCreate" joint-group-id
  (max-size :int))

(defcfun "dJointGroupDestroy" :void
  (joint-group-id joint-group-id))


;; Clear
(defcfun "dJointGroupEmpty" :void
  (joint-group-id joint-group-id))


(defcfun "dJointAttach" :void
  (joint-id joint-id)
  (body-id-1 body-id)
  (body-id-2 body-id))
