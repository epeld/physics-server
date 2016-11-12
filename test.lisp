   
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
  (vector (:pointer vector3)))

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
;; Resource Management
;;


;; This is the managed world id!
;; TODO define managed resources in a separate lisp file!
;(defctype blub (:wrapper :pointer
;                         :from-c pointer-to-id
;                         :to-c id-to-pointer))

;(defcfun ("dWorldCreate" create-world-managed) blub)

;(create-world-managed)

