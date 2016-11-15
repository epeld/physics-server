
;;
;; This is elisp code for traversing ODE header files and extracting a data structure
;; for generating the CL interface
;;

(defvar ode-dir "/usr/local/include/ode")

;;
;;  Regex helpers
;; 
(defun all-matches (regex string &optional start)
  "Find all matches of regex in string, returning it as a list"
  (let ((next (or start 0))
        result)
    (while (string-match regex string next)
      (setq next (match-end 0))
      (push (match-string-no-properties 0 string) result))
    (reverse result)))


(defun split (string)
  (all-matches "\\S-+" string))


(defun comma-split (string)
  (all-matches "[^,]+" string))

(defun trim (string)
  (setq string (replace-regexp-in-string "^\\(\n\\|\\s-\\)+" "" string))
  (setq string (replace-regexp-in-string "\\(\n\\|\\s-\\)$" "" string))
  string)



;;
;;  ODE API parsing
;;

(defun ctype (string &optional array)
  (if (string-match "\\(const\\s-+\\)?\\([a-zA-Z0-9]+\\)" string)
      (if (or array (search "*" string))
          (list :pointer (trim (match-string 2 string)))
        (trim (match-string 2 string))) 
    (trim string)))


(defun cfun-arg (arg)
  "Parse a C-function argument definition"
  (setq arg (trim arg))
  (setq arg (replace-regexp-in-string "/\\*.*\\*/" "" arg))
  (setq arg (trim arg))
  (if (equal (subseq arg (- (length arg) 1)) "*")
      (ctype arg)
    (progn
      (string-match "^\\(.+?\\)\\([a-zA-Z0-9]+\\)\\(\\[[0-9]\\]\\)?$" arg)
      (let ((type (match-string 1 arg))
            (name (match-string 2 arg)))
        (list :type (ctype type (match-string 3 arg)) 
              :name (trim name))))))


(defun cfun-args (string)
  (if (string-match "^\\s-*\\([a-zA-Z]+\\)\\( \\*?\\)\\s-*$" string)
      (list :type (ctype (concat (match-string 1 string) (match-string 2 string)))
            :name "arg1")
    
    (if (or (string-match "^\\s-*void\\s-*$" string)
            (string-match "^\\s-*$" string))
        nil

      (mapcar #'cfun-arg (comma-split string)))))



(defun ode-cfun (string)
  "Parse a single ODE_API definition"
  "ODE_API\\([^(]+\\)(\\(.*\\))"
  (let (args fun parts)
    (unless (string-match "ODE_API\\([^(]+\\)(\\([^)]*\\))" string)
      (error "Not a valid function %s" string))
    (setq fun (match-string 1 string))
    (setq args (match-string 2 string))
    (setq parts (split fun))
    (list :return-type (ctype (nth 0 parts))
          :name (trim (nth 1 parts))
          :args (cfun-args args))))


(defun list-ode-headers (&optional ode-dir)
  (directory-files (or ode-dir "/usr/local/include/ode/") t "\.h$"))


(defun file-contents (path)
  (with-temp-buffer
   (insert-file-contents path)
   (buffer-string)))


(defun header-to-ode-api (header-path)
  "Given a path to a ODE header file, extract its API definitions"
  (let ((m (all-matches "ODE_API\\(.\\|\n.\\)*;" (file-contents header-path))))

    ;; Look for all functions that don't use varargs
    (list :name (file-name-base header-path)
          :functions (mapcar #'ode-cfun (remove-if (lambda (s) (or (search "..." s)
                                                                   (search "dPrintMatrix" s)
                                                                   (search "UseSharedWorkingMemory" s)
                                                                   (search "ReservationPolicy" s)))
                                                   m)))))


(defun generate-ode-api ()
  (interactive)
  "Generate the API declaration for ODE"
  (let (ode-api headers)

    (setq headers (list-ode-headers ode-dir))
    (setq ode-api (mapcar #'header-to-ode-api headers))
  
    (switch-to-buffer-other-window "ode-api")
    (with-current-buffer "ode-api"
      (erase-buffer)
      (prin1 ode-api (current-buffer)))))

(generate-ode-api)
