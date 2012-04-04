#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle.app
  (:use :cl
        :anaphora
        :clack
        :clack.request
        :ningle.middleware.context)
  (:import-from :clack.util.route
                :<url-rule>
                :match
                :make-url-rule)
  (:import-from :ningle.context
                :*request*
                :*response*))
(in-package :ningle.app)

(cl-syntax:use-syntax :annot)

(defparameter *next-route-function* nil
  "A function called when `next-route' is invoked. This will be overwritten in `dispatch-with-rules'.")

@export
(defclass <app> (<component>)
     ((routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules))
  (:documentation "Base class for Ningle Application. All Ningle Application must inherit this class."))

(defmethod call :around ((this <app>) env)
  (call (wrap
         (make-instance '<ningle-middleware-context>)
         (lambda (env)
           (call-next-method this env)))
        env))

(defmethod call ((this <app>) env)
  "Overriding method. This method will be called for each request."
  @ignore env
  (dispatch-with-rules (reverse (routing-rules this))))

(defun dispatch-with-rules (rules)
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (acond
      ((and rules
            (member-rule path-info method rules))
       (destructuring-bind ((url-rule controller) &rest other-rules) it
         (let ((*next-route-function* #'(lambda () (dispatch-with-rules other-rules))))
           (multiple-value-bind (_ params)
               (match url-rule method path-info)
             @ignore _
             (setf (slot-value req 'clack.request::query-parameters)
                   (append
                    params
                    (slot-value req 'clack.request::query-parameters)))
             (if (functionp controller)
                 (funcall controller (parameter req))
                 controller)))))
      (t (not-found)))))

@export
(defmethod route ((this <app>) url-rule &key (method :get))
  (second
   (find-if #'(lambda (rule) (match-url-rule-p rule url-rule method))
            (routing-rules this)
            :key #'car)))

@export
(defmethod (setf route) (controller (this <app>) url-rule &key (method :get))
  (setf (routing-rules this)
        (delete-if #'(lambda (rule) (match-url-rule-p rule url-rule method))
                   (routing-rules this) :key #'car))

  (push (list (make-url-rule url-rule :method method)
              controller)
        (routing-rules this))

  controller)

@export
(defun next-route ()
  (funcall *next-route-function*))

@export
(defun not-found ()
  "An action when no routing rules are found."
  (setf (clack.response:status *response*) 404)
  nil)

(defmethod match-url-rule-p ((rule <url-rule>) url-rule method)
  (and (eq (clack.util.route::request-method rule) method)
       (string= (clack.util.route::url rule) url-rule)))

(defun member-rule (path-info method rules)
  (member-if #'(lambda (rule)
                 (or (match rule method path-info)
                     (and (eq method :HEAD)
                          (match rule :GET path-info))))
             rules
             :key #'car))

(doc:start)

@doc:NAME "
Ningle.App - Ningle Application Class.
"

@doc:SYNOPSIS "
    (defclass <myapp-app> (<app>) ())
    (defvar *app* (make-instance '<myapp-app>))
    
    (setf (route *app* \"/\")
          \"Welcome to ningle!\")
    
    (setf (ningle:route *app* \"/login\" :method :POST)
          #'(lambda (params)
              (if (authorize (getf params :|username|)
                             (getf params :|password|))
                \"Authorized!\"
                \"Failed...Try again.\")))
    
    (call *app*)
"

@doc:DESCRIPTION "
Ningle.App provides a base class `<app>' for Ningle Applications.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
