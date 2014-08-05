(in-package :cl-user)
(defpackage ningle
  (:use :cl)
  (:import-from :ningle.app
                :<app>
                :route
                :requirement
                :next-route
                :not-found
                :clear-routing-rules)
  (:import-from :ningle.context
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables
                :make-request
                :make-response)
  (:export :<app>
           :route
           :requirement
           :next-route
           :not-found
           :clear-routing-rules
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables
           :make-request
           :make-response))
(in-package :ningle)
