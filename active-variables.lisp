;;
;; cl-active-variables: variables with read/write callbacks
;;
;; An example (see the README for a better explanation):
;;
;; CL-USER> (load "active-variables.lisp")
;; T
;; CL-uSER> (in-package :active-variables)
;; #<PACKAGE "ACTIVE-VARIABLES">
;; CL-USER> (defactive x 0
;;  :write-handler (lambda (old-val new-val)
;;		   (format t "old: ~A  new: ~A" old-val new-val)
;;		   new-val)
;;  :read-handler (lambda (val)
;;		  (format t "value: ~A" val)
;;		  val))
;; 0
;; CL-USER> $x ; get value and call :read-handler
;; value: 0
;; 0
;; CL-USER> (setf $x 100) ; set value calling :write-handler
;; old: 0  new: 100
;; 100
;; CL-USER> $x ; get value and call :read-handler
;; value: 100
;; 100
;; CL-USER> x ; just get variable without calling :read-handler
;; 100
;; CL-USER> (setf x 200) ; set value without calling :write-handler
;; 200
;; CL-USER> $x : get value and call :read-handler
;; value: 200
;; 200
;;
;; Author: Burton Samograd <burton.samograd@gmail.com>
;; Year: 2012

(defpackage :active-variables
  (:use :cl)
  (:nicknames "ACTIVE")
  (:export "DEFACTIVE" "SETACTIVE"))
(in-package :active-variables)

(defconstant +active-variable-prefix+ #\$)

(defparameter *active-variables* (make-hash-table))

(set-macro-character +active-variable-prefix+
                    (lambda (stream char)
                      (declare (ignore char))
		      (let ((v (read stream)))
			;; check if we are actually reading an active variable
			(if (gethash v *active-variables*)
			    (list (get v :setf-handler-name))
			    ;; we aren't so just return the rebuilt symbol
			    (intern (format nil "$~A" v)))))
		    t)

(defmacro defactive (var value &key write-handler read-handler)
  "Define an active variable.  An active variable is one which calls a
function when read or written to.  To read an active variable and call
it's read handler, place a $ before it's name.  To write an active
variable and call it's write handler, use (setf $var ...).  To read
and set the variable without calling any handler functions, do not use
the previx $ character.  The variable should be defined without the $
prefix, like so (defactive x ...) and read using $x and written to
using (setf $x ...).  You can define handlers using this function with
the :read-handler and :write-handler keywords, or using setactive."
 (let ((setf-handler-name (gensym)))
  `(progn
     ;; add the variable to the active variables list
     (setf (gethash ',var *active-variables*) t)
     ;; declare the var and the value
     (defparameter ,var ,value)
     ;; define the setf handler name which wrapped around the variable
     ;; when it is is read using $var notation
     (defmacro ,setf-handler-name ()
       (let ((read-handler (gensym)))
         `(let ((,read-handler (get ',',var :read-handler)))
            (if ,read-handler
                (funcall ,read-handler ,',var)
                ,',var))))
     ;; define a handler to call the write handler for when you do a
     ;; (setf $var ...)
     (defsetf ,setf-handler-name () (new-val)
       (let ((write-handler (gensym)))
         `(let ((,write-handler (get ',',var :write-handler)))
            (when ,write-handler
              (setf ,',var (funcall ,write-handler ,',var ,new-val))))))
     ;; save all the info in the variable
     (setf (get ',var :setf-handler-name) ',setf-handler-name)
     (setf (get ',var :write-handler) ,write-handler)
     (setf (get ',var :read-handler) ,read-handler)
     ;; return the value
     ,value)))

(defmacro setactive (var &key read-handler write-handler)
  "helper function for setting :read-handler and :write-handler of the
active variable var"
  `(progn
     (when ,read-handler
       (setf (get ',var :read-handler) ,read-handler))
     (when ,write-handler
       (setf (get ',var :write-handler) ,write-handler))))
