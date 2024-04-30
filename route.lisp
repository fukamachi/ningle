(defpackage #:ningle/route
  (:nicknames #:ningle.route)
  (:use #:cl)
  (:import-from #:myway
                #:route
                #:equal-route
                #:match-route)
  (:export #:ningle-route
           #:route-controller))
(in-package #:ningle/route)

(defclass ningle-route (route)
  ((requirements :initarg :requirements
                 :initform '()
                 :accessor route-requirements)
   (compiled-requirements :initarg :compiled-requirements
                          :accessor route-compiled-requirements)
   (controller :initarg :controller
               :accessor route-controller)))

(defmethod initialize-instance :after ((route ningle-route) &key requirements-map requirements &allow-other-keys)
  (when requirements-map
    (setf (route-compiled-requirements route)
          (compile-requirements requirements-map requirements))))

(defmethod equal-route ((route1 ningle-route) (route2 ningle-route))
  (and (equal (route-requirements route1)
              (route-requirements route2))
       (call-next-method)))

(defmethod match-route ((route ningle-route) method url-string &key allow-head)
  (declare (ignore method url-string allow-head))
  (multiple-value-bind (requirement-satisfied requirement-params)
      (funcall (route-compiled-requirements route))
    (when requirement-satisfied
      (multiple-value-bind (matchp params)
          (call-next-method)
        (values matchp
                (append requirement-params params))))))

(defun compile-requirements (map requirements)
  (let ((compiled (loop for (name val) on requirements by #'cddr
                        for fn = (gethash name map)
                        if fn
                        collect (lambda ()
                                  (multiple-value-bind (satisfied res)
                                      (funcall fn val)
                                    (and satisfied
                                         (list name res))))
                        else
                        do (error "Requirement ~S is not defined." name))))
    (lambda ()
      (loop for fn in compiled
            for result = (funcall fn)
            if result
            append result into params
            else
            do (return nil)
            finally
               (return (values t params))))))
