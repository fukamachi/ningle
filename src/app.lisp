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
                :match)
  (:import-from :ningle.context
                :*request*
                :*response*))
(in-package :ningle.app)

(cl-syntax:use-syntax :annot)

@export
(defclass <ningle-app> (<component>)
     ((routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules))
  (:documentation "Base class for Ningle Application. All Ningle Application must inherit this class."))

(defmethod call :around ((this <ningle-app>) env)
  (call (wrap
         (make-instance '<ningle-middleware-context>)
         (lambda (env)
           (call-next-method this env)))
        env))

(defmethod call ((this <ningle-app>) env)
  "Overriding method. This method will be called for each request."
  @ignore env
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (loop for (nil rule fn) in (reverse (routing-rules this))
          do (multiple-value-bind (matchp params)
                 (match rule method path-info)
               (when matchp
                 (setf (slot-value req 'clack.request::query-parameters)
                       (append
                        params
                        (slot-value req 'clack.request::query-parameters)))
                 (let ((res (call fn (parameter req))))
                   (unless (eq res (next-route))
                     (return res)))))
          finally
          (progn (setf (clack.response:status *response*) 404)
                 nil))))

@export
(defmethod add-route ((this <ningle-app>) routing-rule)
  "Add a routing rule to the Application."
  (setf (routing-rules this)
        (delete (car routing-rule)
                (routing-rules this)
                :key #'car))
  (push routing-rule
        (routing-rules this)))

(defparameter +next-route+ '#:next-route)

@export
(defun next-route ()
  +next-route+)

@export
(defmethod lookup-route ((this <ningle-app>) symbol)
  "Lookup a routing rule with SYMBOL from the application."
  (loop for rule in (reverse (routing-rules this))
        if (eq (first rule) symbol) do
          (return rule)))

(doc:start)

@doc:NAME "
Ningle.App - Ningle Application Class.
"

@doc:SYNOPSIS "
    (defclass <myapp-app> (<ningle-app>) ())
    (defvar *app* (make-instance '<myapp-app>))
    (call *app*)
"

@doc:DESCRIPTION "
Ningle.App provides a base class `<ningle-app>' for Ningle Applications.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
