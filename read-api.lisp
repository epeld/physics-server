
;;
;; Script for reading in an api definition generated by extract-api.el
;; 

(in-package :c-api-info)

(defparameter builtins
  '("char"
    "short"
    "float"
    "double"
    "int"
    "void"
    "long"
    "file")
  
  "List of built-in C data types")


(defparameter interface-data
  (with-open-file (s "~/Documents/Code/physics-server/ode-api.lisp")
    (read s))

  "A data structure representing the C-interface")


(defparameter excluded-modules
  '("odecpp"
    "odecpp_collision"
    "threading"
    "timer"
    "memory")

  "Modules we are not interested in interfacing with")


(defun function-name (f)
  "Extract the name of a function data object"
  (getf f :name))


(defparameter function-exclusion-substrings
  '("tri" "thread" "memory" "geomClass" "heightfield")
  
  "Exclude all functions that contain these substrings in their names")

(defparameter function-exclusion-predicates
  (list (lambda (f)
          (loop for s in function-exclusion-substrings
             thereis (search s (function-name f) :test #'char-equal)))))


(defun module-functions (m)
  "Extract all valid functions from a module data object"
  (remove-if (lambda (f)
               (loop for p in function-exclusion-predicates
                    thereis (funcall p f)))
             (getf m :functions)))


(defun unpointerify (type-decl)
  "Keep normal type declarations intact but remove pointer declarations"
  (if (and (consp type-decl)
           (eq :pointer (first type-decl)))
      (second type-decl)
      type-decl))


(defun get-arg-type (x)
  (getf (the list x) :type))


(defun get-fun-arg-types (f)
  (mapcar #'get-arg-type (getf f :args)))


(defun remove-equal-duplicates (x)
    (remove-duplicates x :test #'equal))


(defun types (&optional (module (first interface-data)))
  "Figure out the set of all possible types that we need to support for this api"
  (the list module)
  (let ((arg-types (loop for f in (module-functions module)
                      nconc (get-fun-arg-types f)))
        
        (return-types (loop for f in (module-functions module)
                         collect (getf f :return-type))))
    
    (mapcar #'unpointerify (append return-types arg-types))))



(defun convert-builtin (type-decl)
  "Convert a builtin C type declaration from string to keyword"
  
  (cond
    ;; 
    ;; Special cases
    ((eq :string type-decl)
     :string)

    ;; 
    ;; Unsigned?
    ((string-equal "unsigned" type-decl) ; Match the string verbatim
     :unsigned-int)

    ;; match the substring
    ((search "unsigned" type-decl)
     (let ((type (convert-builtin (subseq type-decl (length "unsigned ")))))
       (when type 
         (find-symbol (string-upcase (format nil "unsigned-~a" type)) (find-package :keyword)))))

    ;; 
    ;; Default case: Look for builtin
    ((find type-decl builtins :test #'string-equal)
     (find-symbol (string-upcase type-decl) (find-package :keyword)))))


(defun get-modules (&optional (api interface-data))
  "Extract all interesting modules from the interface data"
  (remove-if (lambda (m)
               (find (getf m :name) excluded-modules :test #'string-equal))
             interface-data))


(defun all-builtin-types (&optional (modules (get-modules)))
  "List all types in the C-interface"
  (let ((strings (remove-equal-duplicates (loop for m in modules nconc (types m)))))
    (remove-if #'null (mapcar #'convert-builtin strings))))


(defun all-custom-types (&optional (modules (get-modules)))
  "List all types in the C-interface"
  (let ((strings (remove-equal-duplicates (loop for m in modules nconc (types m)))))
    (remove-if #'convert-builtin strings)))


(defun all-callback-types (&optional (modules (get-modules)))
  "List all functions/callback types that the C interface needs"
  (remove-if-not (lambda (type)
                   (or (search "fn" type :test #'string-equal)
                       (search "Function" type :test #'string-equal)
                       (search "Callback" type :test #'string-equal)))
                 (all-custom-types modules)))


(defun all-id-types (&optional (modules (get-modules)))
  (remove-if-not (lambda (x) (search "ID" x)) (all-custom-types)))

;interface-data

;; (all-builtin-types)
;; (all-callback-types)
;; (all-id-types)
;; (all-custom-types)







