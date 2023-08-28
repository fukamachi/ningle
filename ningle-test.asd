(defsystem "ningle-test"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :defsystem-depends-on ("prove")
  :depends-on ("ningle"
               "prove"
               "lack-test"
               "lack-component"
               "yason"
               "babel")
  :components ((:module "tests"
                :components
                ((:test-file "main")
                 (:test-file "requirements"))))
  :perform (test-op (op c) (symbol-call :prove :run-test-system c)))
