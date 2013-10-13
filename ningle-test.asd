#|
  This file is a part of ningle project.
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage ningle-test-asd
  (:use :cl :asdf))
(in-package :ningle-test-asd)

(defsystem ningle-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:ningle
               :cl-test-more
               :clack-test
               :drakma
               :yason
               :babel)
  :components ((:module "t"
                :components
                ((:file "ningle"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
