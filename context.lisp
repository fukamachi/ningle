(defpackage #:ningle/context
  (:nicknames #:ningle.context)
  (:use #:cl)
  (:export #:*context*
           #:*request*
           #:*response*
           #:*session*
           #:make-context
           #:make-request
           #:make-response
           #:context
           #:with-context-variables))
(in-package #:ningle/context)

(defvar *context* nil
  "Special variable to store Ningle Context, a hash table.
Don't set to this variable directly. This is designed to be bound in lexical let.")

(defvar *request* nil
  "Special variable to store Ningle Request, a instance of `<request>' in Ningle.Request package.
Don't set to this variable directly. This is designed to be bound in lexical let.")

(defvar *response* nil
  "Special variable to store Ningle Response, a instance of `<response>' in Ningle.Response package.
Don't set to this variable directly. This is designed to be bound in lexical let.")

(defvar *session* nil
  "Special variable to store session.
Don't set to this variable directly. This is designed to be bound in lexical let.")

(defun make-context (app env)
  "Create a new Context."
  (let ((*context* (make-hash-table)))
    (setf (context :request) (make-request app env)
          (context :response) (make-response app 200 ())
          (context :session) (getf env :lack.session))
    *context*))

(defgeneric make-request (app env)
  (:documentation "Make a request object. See ningle.app for the default behavior."))

(defgeneric make-response (app &optional status headers body)
  (:documentation "Make a response object. See ningle.app for the default behavior."))

(defun context (&optional key)
  "Access to current context. If key is specified, return the value in current context.
If not, just return a current context.

Example:
  (context)
  ;=> #<HASH-TABLE :TEST EQL size 0/60 #x3020025FF5FD>
  (context :request)
  ;=> #<CAVEMAN.REQUEST:<REQUEST> #x3020024FCCFD>"
  (if key (gethash key *context*) *context*))

(defun (setf context) (val key)
  (setf (gethash key *context*) val))

(defmacro with-context-variables ((&rest vars) &body body)
  `(symbol-macrolet
       ,(loop for var in vars
              for form = `(context ,(intern (string var) :keyword))
              collect `(,var ,form))
     ,@body))
