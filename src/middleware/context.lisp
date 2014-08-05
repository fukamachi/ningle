(in-package :cl-user)
(defpackage ningle.middleware.context
  (:use :cl
        :clack)
  (:import-from :ningle.context
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :make-context)
  (:import-from :clack.response
                :body
                :finalize))
(in-package :ningle.middleware.context)

(cl-syntax:use-syntax :annot)

@export
(defclass <ningle-middleware-context> (<middleware>)
  ((last-app :initarg :last-app
             :reader last-app))
  (:documentation "Clack Middleware to set context for each request."))

(defmethod call ((this <ningle-middleware-context>) env)
  (let* ((*context* (make-context (last-app this) env))
         (*request* (context :request))
         (*response* (context :response))
         (*session* (context :session))
         (result (call-next this env)))
    (cond
      ((and result (listp result))
       result)
      (result
       (setf (body *response*) result)
       (finalize *response*))
      (t
       (finalize *response*)))))

(doc:start)

@doc:NAME "
Ningle.Middleware.Context - Clack Middleware to set context for each request.
"

@doc:DESCRIPTION "
This is a Clack Middleware to ensure context is set for each request.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Ningle.Context
"
