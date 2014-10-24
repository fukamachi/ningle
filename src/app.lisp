(in-package :cl-user)
(defpackage ningle.app
  (:use :cl
        :cl-annot.doc
        :clack
        :clack.request
        :ningle.middleware.context)
  (:shadowing-import-from :ningle.context
                          :*request*
                          :*response*
                          :make-request
                          :make-response)
  (:import-from :ningle.route
                :ningle-route
                :route-controller)
  (:import-from :myway
                :make-mapper
                :add-route
                :find-route
                :dispatch)
  (:import-from :alexandria
                :delete-from-plist))
(in-package :ningle.app)

(cl-syntax:use-syntax :annot)

(defvar *default-requirements-map*
  (let ((hash (make-hash-table :test 'eq)))
    (setf (gethash :accept hash)
          (lambda (types)
            (let ((accept-header (gethash "accept" (headers *request*))))
              (and accept-header
                   (some (lambda (type)
                           (ppcre:scan (format nil "(?i)\\b~A\\b" type) accept-header))
                         (if (listp types)
                             types
                             (list types)))))))
    hash))

@export
(defclass <app> (<component>)
  ((mapper :initform (make-mapper)
           :accessor mapper)
   (requirements :type hash-table
                 :initform *default-requirements-map*
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
  (multiple-value-bind (res foundp)
      (dispatch (mapper this) (path-info *request*)
                :method (request-method *request*))
    (if foundp
        res
        (not-found this))))

@export
(defgeneric route (app string-url-rule &rest args &key method identifier regexp &allow-other-keys)
  (:method ((this <app>) string-url-rule &rest args &key (method :get) identifier regexp &allow-other-keys)
    (let ((route
            (find-route (mapper this) string-url-rule
                        :method method
                        :regexp regexp
                        :name identifier
                        :requirements (delete-from-plist args
                                                         :method :identifier :regexp)
                        :route-class 'ningle-route)))
      (if route
          (route-controller route)
          nil))))

@export
(defgeneric (setf route) (controller app string-url-rule &rest args &key method identifier regexp &allow-other-keys)
  (:method (controller (this <app>) string-url-rule &rest args &key (method :get) identifier regexp &allow-other-keys)
    (let ((requirements (delete-from-plist args
                                           :method :identifier :regexp)))
      (add-route (mapper this)
                 (make-instance 'ningle-route
                                :url string-url-rule
                                :method method
                                :regexp regexp
                                :handler (typecase controller
                                           ((or symbol function)
                                            (lambda (params)
                                              (funcall controller
                                                       (append (parameter *request*)
                                                               (loop for (k v) on params by #'cddr
                                                                     collect (cons k v))))))
                                           (T controller))
                                :controller controller
                                :name identifier
                                :requirements requirements
                                :requirements-map (app-requirements this))))

    controller))

@export
(defun requirement (app name)
  (check-type app <app>)
  (gethash name (app-requirements app)))

@export
(defun (setf requirement) (fn app name)
  (check-type app <app>)
  (setf (gethash name (app-requirements app)) fn))

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
  (setf (mapper app) (make-mapper)))

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
              (if (authorize (cdr (assoc \"username\" params :test #'string=))
                             (cdr (assoc \"password\" params :test #'string=)))
                  \"Authorized!\"
                  \"Failed...Try again.\")))
    
    (call *app*)
"

@doc:DESCRIPTION "
Ningle.App provides a base class `<app>' for Ningle Applications.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
