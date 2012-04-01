#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle
  (:use :cl)
  (:import-from :ningle.route
                :url
                :url-for)
  (:import-from :ningle.app
                :<ningle-app>
                :next-route)
  (:import-from :ningle.context
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables)
  (:export :url
           :url-for
           :<ningle-app>
           :next-route
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables))
(in-package :ningle)

;; blah blah blah.
