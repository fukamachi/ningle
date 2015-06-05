#|
  This file is a part of ningle project.
  Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle-test-asd
  (:use :cl :asdf))
(in-package :ningle-test-asd)

(defsystem ningle-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:ningle
               :prove
               :clack-test
               :drakma
               :yason
               :babel)
  :components ((:module "t"
                :components
                ((:test-file "ningle")
                 (:test-file "requirements"))))

  :defsystem-depends-on (:prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove)
                             c)))
