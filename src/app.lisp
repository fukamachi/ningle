#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle.app
  (:use :cl
        :cl-annot.doc
        :clack
        :clack.request
        :ningle.middleware.context)
  (:import-from :clack.util.route
                :<url-rule>
                :<regex-url-rule>
                :match
                :make-url-rule)
  (:shadowing-import-from :ningle.context
                          :*request*
                          :*response*
                          :make-request
                          :make-response))
(in-package :ningle.app)

(cl-syntax:use-syntax :annot)

(defvar *requirement-map*
  (let ((hash (make-hash-table :test 'eq)))
    (setf (gethash :accept hash)
          (lambda (types)
            (let ((accept-header (getf (env *request*) :http-accept)))
              (some (lambda (type)
                      (ppcre:scan (format nil "(?i)\\b~A\\b" type) accept-header))
                    (if (listp types)
                        types
                        (list types))))))
    hash))

(defstruct (routing-rule (:constructor make-routing-rule (url-rule
                                                          controller
                                                          &optional
                                                            identifier
                                                            requirements
                                                            compiled-requirements)))
  (url-rule nil :type <url-rule>)
  controller
  identifier
  (requirements '() :type list)
  compiled-requirements)

(defparameter *next-route-function* nil
  "A function called when `next-route' is invoked. This will be overwritten in `dispatch-with-rules'.")

@export
(defclass <app> (<component>)
  ((routing-rules :type list
                  :initarg :routing-rules
                  :initform '()
                  :accessor routing-rules)
   (requirements :type hash-table
                 :initform (make-hash-table :test 'eq)
                 :accessor app-requirements)
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
  (let ((res (dispatch-with-rules (reverse (routing-rules this)))))
    (if (eq res :not-found)
        (not-found this)
        res)))

(defun dispatch-with-rules (rules)
  (let ((path-info (path-info *request*))
        (method (request-method *request*)))
    (loop for (rule . rules) on rules
          do (tagbody
                (multiple-value-bind (matchp params)
                    (match (routing-rule-url-rule rule) method path-info :allow-head t)
                  (when matchp
                    (let ((req-params
                            (loop for (name requirement) on (routing-rule-compiled-requirements rule) by #'cddr
                                  for (ok val) = (multiple-value-list (funcall requirement))
                                  if ok
                                    append (list name val)
                                  else
                                    do (go next))))
                      (setf params (nconc params req-params)))
                    (let ((*next-route-function* #'(lambda () (dispatch-with-rules rules)))
                          (controller (routing-rule-controller rule)))
                      (return
                        (typecase controller
                          (function (funcall controller
                                             (append params (parameter *request*))))
                          (symbol (funcall (symbol-function controller)
                                           (append params (parameter *request*))))
                          (T controller))))))
              next)
          finally (return :not-found))))

@export
(defmethod route ((this <app>) string-url-rule &rest requirements &key (method :get) identifier regexp &allow-other-keys)
  (setf requirements
        (loop for (k v) on requirements by #'cddr
              unless (member k '(:method :identifier :regexp) :test #'eq)
                append (list k v)))
  (let ((matched-rule
          (find-if #'(lambda (rule)
                       (match-routing-rule-p rule string-url-rule method
                                             :identifier identifier
                                             :regexp regexp
                                             :requirements requirements))
                   (routing-rules this))))
    (if matched-rule
        (routing-rule-controller matched-rule)
        nil)))

@export
(defmethod (setf route) (controller (this <app>) string-url-rule &rest requirements &key (method :get) identifier regexp &allow-other-keys)
  (setf requirements
        (loop for (k v) on requirements by #'cddr
              unless (member k '(:method :identifier :regexp) :test #'eq)
                append (list k v)))
  (setf (routing-rules this)
        (delete-if #'(lambda (rule)
                       (match-routing-rule-p rule
                                             string-url-rule
                                             method
                                             :controller controller
                                             :identifier identifier
                                             :requirements requirements))
                   (routing-rules this)))

  (push (make-routing-rule (make-url-rule string-url-rule :method method :regexp regexp)
                           controller
                           identifier
                           requirements
                           (compile-requirements this requirements))
        (routing-rules this))

  controller)

(defun compile-requirements (app requirements)
  (loop for (k v) on requirements by #'cddr
        for requirement = (or (gethash k (app-requirements app))
                              (gethash k *requirement-map*))
        if requirement
          append (list k (lambda () (funcall requirement v)))
        else
          do (error "Routing requirement \"~S\" is not found." k)))

@export
(defun requirement (app name)
  (check-type app <app>)
  (gethash name (app-requirements app)))

@export
(defun (setf requirement) (new app name)
  (setf (gethash name (app-requirements app)) new))

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

@export
(defun clear-routing-rules (app)
  (setf (routing-rules app) '()))

(defmethod match-routing-rule-p ((rule routing-rule) string-url-rule method &key controller identifier regexp requirements)
  (declare (ignore controller))
  (let ((url-rule (routing-rule-url-rule rule)))
    (and (eq identifier (routing-rule-identifier rule))
         (equal (clack.util.route::request-method url-rule) method)
         (string= (clack.util.route::url url-rule) string-url-rule)
         (eq regexp (typep url-rule '<regex-url-rule>))
         (equal requirements (routing-rule-requirements rule)))))

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
