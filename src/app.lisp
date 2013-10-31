#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle.app
  (:use :cl
        :cl-annot.doc
        :anaphora
        :clack
        :clack.request
        :ningle.middleware.context)
  (:import-from :clack.util.route
                :<url-rule>
                :match
                :make-url-rule)
  (:shadowing-import-from :ningle.context
                          :*request*
                          :*response*
                          :make-request
                          :make-response))
(in-package :ningle.app)

(cl-syntax:use-syntax :annot)

(defstruct (routing-rule (:constructor make-routing-rule (url-rule controller &optional identifier)))
  (url-rule nil :type <url-rule>)
  controller
  identifier)

(defparameter *next-route-function* nil
  "A function called when `next-route' is invoked. This will be overwritten in `dispatch-with-rules'.")

@export
(defclass <app> (<component>)
     ((routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules)
      (%context-mw))
  (:documentation "Base class for Ningle Application. All Ningle Application must inherit this class."))

(defmethod initialize-instance :after ((this <app>) &rest args)
  (declare (ignore args))
  (setf (slot-value this '%context-mw)
        (make-instance '<ningle-middleware-context>
                       :last-app this)))

(defmethod call :around ((this <app>) env)
  (call (wrap
         (slot-value this '%context-mw)
         (lambda (env)
           (call-next-method this env)))
        env))

(defmethod call ((this <app>) env)
  "Overriding method. This method will be called for each request."
  @ignore env
  (or (dispatch-with-rules (reverse (routing-rules this)))
      (not-found this)))

(defun dispatch-with-rules (rules)
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (aif (and rules
              (member-rule path-info method rules :allow-head t))
         (destructuring-bind (routing-rule &rest other-rules) it
           (let ((*next-route-function* #'(lambda () (dispatch-with-rules other-rules))))
             (multiple-value-bind (_ params)
                 (match (routing-rule-url-rule routing-rule) method path-info :allow-head t)
               @ignore _
               (let ((controller (routing-rule-controller routing-rule)))
                 (typecase controller
                   (function (funcall controller
                                      (append params (parameter req))))
                   (symbol (funcall (symbol-function controller)
                                    (append params (parameter req))))
                   (T controller))))))
         nil)))

@export
(defmethod route ((this <app>) string-url-rule &key (method :get) identifier)
  (let ((matched-rule
          (find-if #'(lambda (rule)
                       (match-routing-rule-p rule string-url-rule method
                                         :identifier identifier))
                   (routing-rules this))))
    (if matched-rule
        (routing-rule-controller matched-rule)
        nil)))

@export
(defmethod (setf route) (controller (this <app>) string-url-rule &key (method :get) identifier)
  (setf (routing-rules this)
        (delete-if #'(lambda (rule)
                       (match-routing-rule-p rule
                                         string-url-rule
                                         method
                                         :controller controller
                                         :identifier identifier))
                   (routing-rules this)))

  (push (make-routing-rule (make-url-rule string-url-rule :method method)
                           controller
                           identifier)
        (routing-rules this))

  controller)

@export
(defun next-route ()
  (funcall *next-route-function*))

@export
(defgeneric not-found (app)
  (:documentation "An action when no routing rules are found."))

@export
(defmethod not-found ((this <app>))
  @ignore this
  (setf (clack.response:status *response*) 404)
  nil)

(defmethod match-routing-rule-p ((rule routing-rule) string-url-rule method &key controller identifier)
  (let ((url-rule (routing-rule-url-rule rule)))
    (if (or identifier (routing-rule-identifier rule))
        (eq identifier (routing-rule-identifier rule))
        (or (and controller
                 (eq (routing-rule-controller rule) controller))
            (and (equal (clack.util.route::request-method url-rule) method)
                 (string= (clack.util.route::url url-rule) string-url-rule))))))

(defun member-rule (path-info method rules &key allow-head)
  (member-if #'(lambda (rule)
                 (match rule method path-info :allow-head allow-head))
             rules
             :key #'routing-rule-url-rule))

@doc "Make a request object. A class of the request object can be changed by overwriting this."
(defmethod make-request ((app <app>) env)
  (clack.request:make-request env))

@doc "Make a response object. A class of the response object can be changed by overwriting this."
(defmethod make-response ((app <app>) &optional status headers body)
  (declare (ignore app))
  (clack.response:make-response status headers body))

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
