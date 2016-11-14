   
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


(defcstruct vector3 
  (x ode-real)
  (y ode-real)
  (z ode-real))


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

(defcfun ("dGeomSetPosition" set-geom-position) :void
  (geom-id geom-id)
  (x ode-real)
  (y ode-real)
  (z ode-real))

(defcfun ("dGeomGetPosition" get-geom-position) (:pointer ode-real)
  (geom-id geom-id))

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

