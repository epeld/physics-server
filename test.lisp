   
(in-package :ode)

(define-foreign-library libode
    (:darwin (:or "libode.3.dylib" "libode.dylib"))
    (:unix (:or "libode.so.3" "libode.so"))
    (t (:default "libode")))
   
(use-foreign-library libode)

;;
;; Info
(defcfun ("dGetConfiguration" get-configuration) :string)

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


(defcfun ("dInitODE2" init) :uint (flags init-flags))
(defcfun ("dCloseODE" uninit) :void)

(defcfun ("dAllocateODEDataForThread" allocate-data-for-thread) :uint (flags allocate-flags))

;; Go ahead and initialize everything directly
;; (assumes we are main thread)
(init '(:none))
(allocate-data-for-thread '(:all))

;;
;;  World API
;; 
(defctype world-id :pointer)
(defctype space-id :pointer)

;; Creation
(defcfun ("dWorldCreate" create-world) world-id)
(defcfun ("dWorldDestroy" destroy-world) :void (world-id world-id))

;; Gravity
(defcfun ("dWorldSetGravity" set-gravity) :void
  (world-id world-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun ("dWorldGetGravity" get-gravity) :void 
  (world-id world-id)
  (vector (:pointer (:struct vector3))))

;; ERP
(defcfun ("dWorldSetERP" set-erp) :void (world-id world-id) (erp ode-real))
(defcfun ("dWorldGetERP" get-erp) ode-real (world-id world-id))

;; CFM
(defcfun ("dWorldSetCFM" set-cfm) :void (world-id world-id) (cfm ode-real))
(defcfun ("dWorldGetCFM" get-cfm) ode-real (world-id world-id))

;; Update / Stepping
(defcfun ("dWorldStep" step-world) :void (world-id world-id) (step-size ode-real))
(defcfun ("dWorldQuickStep" quick-step-world) :void (world-id world-id) (step-size ode-real))

;;
;; Body
;; 
(defctype body-id :pointer)

(defcfun ("dBodyCreate" create-body) body-id
  (world-id world-id))

(defcfun ("dBodyDestroy" destroy-body) body-id
  (body-id body-id))

;;
;; Geom
;; 
(defctype geom-id :pointer)

;; Creation
(defcfun ("dCreateBox" create-box) geom-id
  (space-id space-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun ("dCreateSphere" create-sphere) geom-id
  (space-id space-id)
  (radius ode-real))

(defcfun ("dCreatePlane" create-plane) geom-id
  (space-id space-id)
  (a ode-real)
  (b ode-real)
  (c ode-real)
  (d ode-real))

(defcfun ("dGeomDestroy" destroy-geom) :void
  (geom-id geom-id))


;; Bodies
(defcfun ("dGeomGetBody" get-geom-body) body-id
  (geom-id geom-id))

(defcfun ("dGeomSetBody" set-geom-body) :void
  (geom-id geom-id)
  (body-id body-id))

;; Position
(defcfun ("dGeomSetPosition" set-geom-position) :void
  (geom-id geom-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun ("dGeomGetPosition" get-geom-position) (:pointer ode-real)
  (geom-id geom-id))


;; Rotation
(defcfun ("dGeomGetRotation" get-geom-rotation) (:pointer (:struct matrix3))
  (geom-id geom-id))


(defcfun ("dGeomSetRotation" set-geom-rotation) :void
  (geom-id geom-id)
  (r (:pointer (:struct matrix3))))

;; 
;; Space
;;
;(defctype space-id :pointer)

(defcfun ("dHashSpaceCreate" create-hash-space) space-id (space-id space-id))

(defcfun ("dSpaceAdd" add-geom-to-space) :void (space-id space-id) (geom-id geom-id))
(defcfun ("dSpaceRemove" remove-geom-from-space) :void (space-id space-id) (geom-id geom-id))
(defcfun ("dSpaceClean" clear-space) :void (space-id space-id))

(defcfun ("dSpaceGetNumGeoms" get-num-geoms) :int (space-id space-id))
(defcfun ("dSpaceGetGeom" get-geom) geom-id (space-id space-id) (index :int))

;;
;; Collision
;;

(defcfun ("dSpaceCollide" space-collide) :void
  (space-id space-id)
  (data :pointer)
  (near-callback :pointer))

;; TODO figure out contact geom format
(defcfun ("dCollide" collide) :int
  (o1 geom-id)
  (o2 geom-id)
  (flags :int)
  (contact :pointer)
  (skip :int))

;;
;;  Rotation
;;

(defcfun ("dRFromAxisAndAngle" rotation-around-axis) :void
  (r (:pointer (:struct matrix3)))
  (ax ode-real)
  (ay ode-real)
  (bz ode-real)
  (angle ode-real))

(defcfun ("dRSetIdentity" set-identity) :void
  (r (:pointer (:struct matrix3))))
