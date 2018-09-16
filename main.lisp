(uiop:define-package #:ningle
  (:nicknames #:ningle/main)
  (:use #:cl)
  (:use-reexport #:ningle/app
                 #:ningle/context)
  (:import-from #:myway
                #:next-route)
  (:export #:next-route))
(in-package #:ningle)
