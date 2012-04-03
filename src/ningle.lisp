#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle
  (:use :cl)
  (:import-from :ningle.app
                :<app>
                :route
                :next-route)
  (:import-from :ningle.context
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables)
  (:export :<app>
           :route
           :next-route
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables))
(in-package :ningle)

;; blah blah blah.
