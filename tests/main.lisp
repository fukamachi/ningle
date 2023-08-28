(defpackage #:ningle/tests/main
  (:use #:cl
        #:ningle
        #:prove)
  (:import-from #:lack.component
                #:to-app)
  (:import-from #:lack.request
                #:request-method
                #:request-uri)
  (:import-from #:lack.response
                #:response-headers
                #:response-body)
  (:import-from #:lack.test
                #:request
                #:testing-app)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:yason
                #:parse))
(in-package #:ningle/tests/main)

(plan 12)

(defvar *app*)
(setf *app* (make-instance 'app))

(ok (not (route *app* "/")))

(setf (route *app* "/")
      (lambda (params)
        (declare (ignore params))
        "Hello, World!"))

(ok (route *app* "/"))
(ok (not (route *app* "/" :method :POST)))

(setf (route *app* "/post" :method :POST)
      (lambda (params)
        (declare (ignore params))
        "posted"))

(ok (not (route *app* "/post")))
(ok (route *app* "/post" :method :POST))

(setf (route *app* "/new" :method '(:GET :POST))
      (lambda (params)
        (declare (ignore params))
        "new"))
(ok (route *app* "/new" :method '(:GET :POST)))

(setf (route *app* "/testfile")
      (lambda (params)
        (declare (ignore params))
        (asdf:system-relative-pathname :ningle-test #P"tests/test.html")))

(setf (route *app* "/hello.json")
      (lambda (params)
        (declare (ignore params))
        '(200 (:content-type "application/json") ("{\"text\":\"Hello, World!\"}"))))

(setf (route *app* "/hello2.json")
      (lambda (params)
        (declare (ignore params))
        (setf (getf (response-headers *response*) :content-type)
              "application/json")
        "{\"text\":\"Hello, World!\"}"))

(defun say-hello (params)
  (format nil "Hello, ~A" (cdr (assoc "name" params :test #'string=))))

(setf (route *app* "/hello")
      'say-hello)

(setf (route *app* "/hello" :identifier 'say-hello)
      #P"hello.html")

(is (route *app* "/hello") 'say-hello)
(is (route *app* "/hello" :identifier 'say-hello) #P"hello.html")

(setf (route *app* "/hello_to/(.+)" :regexp t)
      (lambda (params)
        (format nil "Saying hello to ~{~A~^ ~}" (cdr (assoc (string :captures) params :test #'string=)))))

(ok (route *app* "/hello_to/(.+)" :regexp t))

(setf (route *app* "/return-nil")
      (lambda (params)
        (declare (ignore params))
        nil))

(testing-app (to-app *app*)
  (is (request "/") "Hello, World!")
  (loop for url in '("/hello.json" "/hello2.json")
        do (multiple-value-bind (body status headers)
               (request url)
             (is status 200)
             (is (gethash "content-type" headers) "application/json")
             (is (gethash "text" (yason:parse body))
                 "Hello, World!")))
  (is (nth-value 1 (request "/testfile")) 200
      "Can return a pathname.")

  (is (request "/hello?name=Eitaro")
      "Hello, Eitaro"
      "Allow a symbol for a controller.")

  (is (request "/hello_to/eitaro/fukamachi")
      "Saying hello to eitaro/fukamachi"
      "Regular expression URL rule.")

  (is (nth-value 1 (request "/return-nil"))
      200))

(defclass ningle-test-app (app) ())
(defstruct (ningle-test-request (:include lack.request:request)))
(defmethod make-request ((app ningle-test-app) env)
  (let ((req (apply #'make-ningle-test-request :allow-other-keys t env)))
    (setf (request-method req) (getf env :request-method))
    (setf (request-uri req) (getf env :request-uri))
    req))

(defvar *app2*)
(setf *app2* (make-instance 'ningle-test-app))

(setf (route *app2* "/request-class")
      (lambda (params)
        (declare (ignore params))
        (let ((*package* (find-package :cl-user)))
          (prin1-to-string (class-name (class-of ningle:*request*))))))

(testing-app (to-app *app2*)
  (is (request "/request-class")
      (format nil "~A::~A"
              :ningle/tests/main
              :ningle-test-request)
      "Can change the class of request."))

(defmethod not-found ((this ningle-test-app))
  (setf (response-body *response*) "Page not found")
  nil)

(testing-app (to-app *app2*)
  (is (request "/404-page-not-found")
      "Page not found"
      "Can change the behavior on 404"))

(finalize)
