
;;
;; This is elisp code for traversing ODE header files and extracting a data structure
;; for generating the CL interface
;; 

;;
;;  Regex helpers
;; 
(defun all-matches (regex string &optional start)
  "Find all matches of regex in string, returning it as a list"
  (if (string-match regex string (or start 0))
        (cons (match-string-no-properties 0 string) (all-matches regex string (match-end 0)))
      nil))


(defun split (string)
  (all-matches "\\S-+" string))


;;
;;  ODE API parsing
;; 
(defun cfun-arg (arg)
  "Parse a C-function argument definition"
  (let ((parts (split arg)))
    (list :type (car parts)
          :name (cadr parts))))


(defun ode-cfun (string)
  "Parse a single ODE_API definition"
  (let (parts args fun)
    (setq parts (all-matches "[^(]+" string))
    (setq args (nth 1 parts))
    (setq fun (nth 0 parts))
    (list :return-type (nth 1 (split fun))
          :name (nth 2 (split fun))
          :args (mapcar #'cfun-arg (all-matches "[^(),;]+" args)))))


(defun list-ode-headers (&optional ode-dir)
  (directory-files (or ode-dir "/usr/local/include/ode/") t "\.h$"))


(defun file-contents (path)
  (with-temp-buffer
   (insert-file-contents path)
   (buffer-string)))


(defun header-to-ode-api (header-path)
  "Given a path to a ODE header file, extract its API definitions"
  (list :name (file-name-base header-path)
        :functions (mapcar #'ode-cfun (all-matches "ODE_API\\(.\\|\n.\\)*;" (file-contents header-path)))))


(defun generate-ode-api ()
  (interactive)
  "Generate the API declaration for ODE"
  (let (ode-api headers)

    (setq headers (list-ode-headers "/usr/local/include/ode"))
    (setq ode-api (mapcar #'header-to-ode-api headers))
  
    (switch-to-buffer-other-window "ode-api")
    (with-current-buffer "ode-api"
      (erase-buffer)
      (prin1 ode-api (current-buffer)))))

;(generate-ode-api)
