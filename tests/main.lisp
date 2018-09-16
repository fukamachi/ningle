(defpackage #:ningle/tests/main
  (:use #:cl
        #:ningle
        #:prove)
  (:import-from #:lack.request
                #:request
                #:request-method
                #:request-uri)
  (:import-from #:lack.response
                #:response-headers
                #:response-body)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:drakma
                #:http-request)
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

(flet ((localhost (path)
         (format nil "http://localhost:~D~A" clack.test:*clack-test-port* path)))
  (clack.test:subtest-app "Test"
      *app*
    (is (drakma:http-request (localhost "/")) "Hello, World!")
    (loop for url in '("/hello.json" "/hello2.json")
          do (multiple-value-bind (body status headers)
                 (drakma:http-request (localhost url))
               (is status 200)
               (is (cdr (assoc :content-type headers))
                   "application/json")
               (is (gethash "text" (yason:parse (babel:octets-to-string body)))
                   "Hello, World!")))
    (is (nth-value 1 (drakma:http-request (localhost "/testfile"))) 200
        "Can return a pathname.")

    (is (drakma:http-request (localhost "/hello?name=Eitaro"))
        "Hello, Eitaro"
        "Allow a symbol for a controller.")

    (is (drakma:http-request (localhost "/hello_to/eitaro/fukamachi"))
        "Saying hello to eitaro/fukamachi"
        "Regular expression URL rule.")

    (is (nth-value 1 (drakma:http-request (localhost "/return-nil")))
        200)))

(defclass ningle-test-app (app) ())
(defstruct (ningle-test-request (:include request)))
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
        (prin1-to-string (class-name (class-of *request*)))))

(clack.test:subtest-app "Test 2"
    *app2*
  (is (drakma:http-request (format nil "http://localhost:~D/request-class"
                                   clack.test:*clack-test-port*))
      (format nil "~A::~A"
              :ningle/tests/main
              :ningle-test-request)
      "Can change the class of request."))

(defmethod not-found ((this ningle-test-app))
  (setf (response-body *response*) "Page not found")
  nil)

(clack.test:subtest-app "Test 3"
    *app2*
  (is (drakma:http-request (format nil "http://localhost:~D/404-page-not-found"
                                   clack.test:*clack-test-port*))
      "Page not found"
      "Can change the behavior on 404"))

(finalize)
