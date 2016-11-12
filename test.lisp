   
(in-package :ode)

(define-foreign-library libode
    (:darwin (:or "libode.3.dylib" "libode.dylib"))
    (:unix (:or "libode.so.3" "libode.so"))
    (t (:default "libode")))
   
(use-foreign-library libode)

;; Note: The definition of real depends on how ODE was compiled.
;; Either float or double
(defctype real :float)

(defcstruct vector3 
  (x real)
  (y real)
  (z real))

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
  (x real)
  (y real)
  (z real))

(defcfun ("dWorldGetGravity" get-gravity) :void 
  (world-id world-id)
  (vector (:pointer vector3)))

;; ERP
(defcfun ("dWorldSetERP" set-erp) :void (world-id world-id) (erp real))
(defcfun ("dWorldGetERP" get-erp) real (world-id world-id))

;; CFM
(defcfun ("dWorldSetCFM" set-cfm) :void (world-id world-id) (cfm real))
(defcfun ("dWorldGetCFM" get-cfm) real (world-id world-id))

;; Update / Stepping
(defcfun ("dWorldStep" step-world) :void (world-id world-id) (step-size real))
(defcfun ("dWorldQuickStep" quick-step-world) :void (world-id world-id) (step-size real))


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

