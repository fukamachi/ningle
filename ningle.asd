#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Super micro framework for Common Lisp.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle-asd
  (:use :cl :asdf))
(in-package :ningle-asd)

(defsystem ningle
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src"
                :components
                ((:file "ningle" :depends-on ("app"))
                 (:file "app" :depends-on ("middleware/context"))
                 (:file "context")
                 (:file "middleware/context" :depends-on ("context")))))
  :description "Super micro framework for Common Lisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op ningle-test))))
