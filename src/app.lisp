#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle.app
  (:use :cl
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
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (loop for (rule controller) in (reverse (routing-rules this))
          do (multiple-value-bind (matchp params)
                 (match rule method path-info)
               (when matchp
                 (setf (slot-value req 'clack.request::query-parameters)
                       (append
                        params
                        (slot-value req 'clack.request::query-parameters)))

                 (if (functionp controller)
                     (let ((res (call controller (parameter req))))
                       (unless (eq res (next-route))
                         (return res)))
                     (return controller))))
          finally
          (progn (setf (clack.response:status *response*) 404)
                 nil))))

(defmethod match-url-rule-p ((rule <url-rule>) url-rule method)
  (and (eq (clack.util.route::request-method rule) method)
       (string= (clack.util.route::url rule) url-rule)))

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

(defparameter +next-route+ '#:next-route)

@export
(defun next-route ()
  +next-route+)

(doc:start)

@doc:NAME "
Ningle.App - Ningle Application Class.
"

@doc:SYNOPSIS "
    (defclass <myapp-app> (<app>) ())
    (defvar *app* (make-instance '<myapp-app>))
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
