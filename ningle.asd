(defsystem "ningle"
  :class :package-inferred-system
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("cl-syntax-annot"
               "ningle/main")
  :description "Super micro framework for Common Lisp."
  :in-order-to ((test-op (test-op "ningle-test"))))

(register-system-packages "lack-component" '(#:lack.component))
(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-response" '(#:lack.response))
