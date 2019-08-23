(defpackage #:ningle/app
  (:nicknames #:ningle.app)
  (:use #:cl)
  (:shadowing-import-from #:ningle/context
                          #:*context*
                          #:*request*
                          #:*response*
                          #:*session*
                          #:context
                          #:make-context
                          #:make-request
                          #:make-response)
  (:import-from #:ningle/route
                #:ningle-route
                #:route-controller)
  (:import-from #:lack.request
                #:request-headers
                #:request-method
                #:request-path-info
                #:request-parameters
                #:request-content-type)
  (:import-from #:lack.response
                #:response-body
                #:response-status
                #:finalize-response)
  (:import-from #:lack.component
                #:lack-component
                #:call
                #:to-app)
  (:import-from #:myway
                #:make-mapper
                #:add-route
                #:find-route
                #:dispatch)
  (:import-from #:alexandria
                #:delete-from-plist))
(in-package #:ningle/app)

(cl-syntax:use-syntax :annot)

(defun default-requirements-map ()
  (let ((hash (make-hash-table :test 'eq)))
    (setf (gethash :accept hash)
          (lambda (types)
            (let ((accept-header (gethash "accept" (request-headers *request*))))
              (and accept-header
                   (some (lambda (type)
                           (ppcre:scan (format nil "(?i)\\b~A\\b" type) accept-header))
                         (if (listp types)
                             types
                             (list types)))))))
    hash))

@export
(defclass app (lack-component)
  ((mapper :initform (make-mapper)
           :accessor mapper)
   (requirements :type hash-table
                 :initform (default-requirements-map)
                 :accessor app-requirements))
  (:documentation "Base class for Ningle Application. All Ningle Application must inherit this class."))
@export '<app>
(setf (find-class '<app>) (find-class 'app))

(defmacro with-context ((context) &body body)
  `(let* ((*context* ,context)
          (*request* (context :request))
          (*response* (context :response))
          (*session* (context :session)))
     ,@body))

(defmethod call :around ((this app) env)
  (let* ((context
           ;; Handle errors mainly while parsing an HTTP request
           ;;   for preventing from 500 ISE.
           (handler-case (make-context this env)
             (error (e)
               (warn "~A" e)
               (return-from call '(400 () ("Bad Request"))))))
         (result (with-context (context)
                   (call-next-method))))
    (if (functionp result)
        (lambda (responder)
          (with-context (context)
            (funcall result (lambda (result)
                              (funcall responder (process-response this result))))))
        (with-context (context)
          (process-response this result)))))

(defmethod call ((this app) env)
  "Overriding method. This method will be called for each request."
  (declare (ignore env))
  (multiple-value-bind (res foundp)
      (dispatch (mapper this) (request-path-info *request*)
                :method (request-method *request*))
    (if foundp
        res
        (not-found this))))

@export
(defgeneric route (app string-url-rule &rest args &key method identifier regexp &allow-other-keys)
  (:method ((this app) string-url-rule &rest args &key (method :get) identifier regexp &allow-other-keys)
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
  (:method (controller (this app) string-url-rule &rest args &key (method :get) identifier regexp &allow-other-keys)
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
                                                       (append (mapc (lambda (pair)
                                                                                 ;; Omit headers & field-metas only in multipart/form-data.
                                                                       (and (consp (cdr pair))
                                                                            (string-equal (request-content-type *request*)
                                                                                          "multipart/form-data")
                                                                            (rplacd pair
                                                                                 (first (cdr pair)))))
                                                                     (request-parameters *request*))
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
  (check-type app app)
  (gethash name (app-requirements app)))

@export
(defun (setf requirement) (fn app name)
  (check-type app app)
  (setf (gethash name (app-requirements app)) fn))

@export
(defgeneric not-found (app)
  (:documentation "An action when no routing rules are found."))

@export
(defmethod not-found ((this app))
  (setf (response-status *response*) 404)
  nil)

@export
(defun clear-routing-rules (app)
  (setf (mapper app) (make-mapper)))

(defmethod make-request ((app app) env)
  "Make a request object. A class of the request object can be changed by overwriting this."
  (lack.request:make-request env))

(defmethod make-response ((app app) &optional status headers body)
  "Make a response object. A class of the response object can be changed by overwriting this."
  (declare (ignore app))
  (lack.response:make-response status headers body))

@export
(defmethod process-response ((app app) result)
  (cond
    ((and result (listp result))
     result)
    (result
     (setf (response-body *response*) result)
     (finalize-response *response*))
    (t
     (finalize-response *response*))))
