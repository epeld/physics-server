
;;
;; This is elisp code for traversing ODE header files and extracting a data structure
;; for generating the CL interface
;; 

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


;;
;;  ODE API parsing
;;

(defvar ode-arg-list-regex "\\s-*\\([a-zA-Z0-9]+\\)\\(\\*\?\\s-\\*\?\\)\\([a-zA-Z]+\\)\\s-*")

(defun cfun-arg (arg)
  "Parse a C-function argument definition"
  (string-match ode-arg-list-regex arg)
  (list :type (if (search "*" (match-string 2 arg))
                  (list :pointer (match-string 1 arg))
                (match-string 1 arg)) 
          :name (match-string 3 arg)))


(defun cfun-args (string)
  (if (string-match "^\\s-*\\([a-zA-Z]+\\)\\s-*$" string)
      (list :type (match-string 1 string)
            :name "arg1")
    
    (if (or (string-match "^\\s-*void\\s-*$" string)
            (string-match "^\\s-*$" string))
        nil

      (let ((m (all-matches ode-arg-list-regex string)))
        (unless m
          (error "Strange args list: %s" string))
        (mapcar #'cfun-arg m)))))


(defun ode-cfun (string)
  "Parse a single ODE_API definition"
  "ODE_API\\([^(]+\\)(\\(.*\\))"
  (let (args fun parts)
    (unless (string-match "ODE_API\\([^(]+\\)(\\([^)]*\\))" string)
      (error "Not a valid function %s" string))
    (setq fun (match-string 1 string))
    (setq args (match-string 2 string))
    (setq parts (split fun))
    (list :return-type (nth 0 parts)
          :name (nth 1 parts)
          :args (cfun-args args))))


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
    (setq ode-api (mapcar #'header-to-ode-api (subseq headers 0 1)))
  
    (switch-to-buffer-other-window "ode-api")
    (with-current-buffer "ode-api"
      (erase-buffer)
      (prin1 ode-api (current-buffer)))))

(generate-ode-api)
