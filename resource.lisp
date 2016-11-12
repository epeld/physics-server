
(in-package :resources)

;;
;; Helper Function
(defun sample (seq)
  "Pick a random element from the sequence"
  (nth (random (length seq))
       seq))


;;
;; UUID Generation
(defvar uuid-chars (coerce "ABCDEFGH123456789" 'list))


(defun uuid ()
  "Generate a new uuid"
  (let (uuid)
    (loop for i from 1 upto 3
       do
         (loop repeat 4 do
              (push (sample uuid-chars) uuid))
       do
         (unless (eql i 3)
           (push #\- uuid)))
    
    (coerce uuid 'string)))


;;
;; Converter Generation
(defun make-pointer-converters (name alist)
  (the string name)
  (the symbol alist)
  (let ((from-name (intern (string-upcase (format nil "~a-to-pointer" name))))
        (to-name (intern (string-upcase (format nil "pointer-to-~a" name))))
        (arg (gensym name))
        (ptr (gensym "pointer")))

    `(progn

       (defun ,to-name (,ptr)
         (let ((,arg (car (rassoc ,ptr ,alist))))
           ;; Generate a new uuid if necessary
           (unless ,arg
             (setf ,arg (uuid))
             (setf ,alist (acons ,arg ,ptr ,alist)))
           ,arg))


       (defun ,from-name (,arg)
         (let ((,ptr (cdr (assoc ,arg ,alist :test #'string-equal))))
           (or ,ptr
               (error "Unknown ~a ~s" ,name ,arg)))))))


(defmacro defun-pointer-converters (name alist)
  (make-pointer-converters name alist))

