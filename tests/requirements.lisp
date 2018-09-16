(defpackage #:ningle/tests/requirements
  (:use #:cl
        #:ningle
        #:prove)
  (:import-from #:clack.test
                #:subtest-app
                #:*clack-test-port*)
  (:import-from #:drakma
                #:http-request))
(in-package #:ningle/tests/requirements)

(plan 5)

(defvar *app*)
(setf *app* (make-instance '<app>))

(setf (route *app* "/" :accept '("text/html" "text/xml"))
      (lambda (params)
        (declare (ignore params))
        "<html><body>Hello, World!</body></html>"))

(setf (route *app* "/" :accept "text/plain")
      (lambda (params)
        (declare (ignore params))
        "Hello, World!"))

(ok (not (route *app* "/")))
(ok (route *app* "/" :accept "text/plain"))

(flet ((localhost (path)
         (format nil "http://localhost:~D~A" clack.test:*clack-test-port* path)))
  (clack.test:subtest-app "Test 1"
      *app*
    (multiple-value-bind (body status)
        (drakma:http-request (localhost "/")
                             :accept "text/plain")
      (is body "Hello, World!")
      (is status 200))

    (multiple-value-bind (body status)
        (drakma:http-request (localhost "/")
                             :accept "text/html")
      (is body "<html><body>Hello, World!</body></html>")
      (is status 200))
    (is (nth-value 1
                   (drakma:http-request (localhost "/")
                                        :accept "application/json"))
        404)))

(setf (requirement *app* :user-agent)
      (lambda (user-agent-regexp)
        (ppcre:scan-to-strings user-agent-regexp
                               (gethash "user-agent" (lack.request:request-headers *request*)))))

(setf (route *app* "/" :user-agent "Songbird/(\\d+\\.\\d+\\.\\d+)")
      (lambda (params)
        (format nil "Songbird ver ~A" (aref (cdr (assoc :user-agent params)) 0))))

(flet ((localhost (path)
         (format nil "http://localhost:~D~A" clack.test:*clack-test-port* path)))
  (clack.test:subtest-app "Test 2"
      *app*
    (is (nth-value 1
                   (drakma:http-request (localhost "/")))
        404)
    (multiple-value-bind (body status)
        (drakma:http-request (localhost "/")
                             :user-agent "Songbird/2.2.0")
      (is status 200)
      (is body "Songbird ver 2.2.0"))))

(isnt (ningle.app::app-requirements (make-instance '<app>))
      (ningle.app::app-requirements (make-instance '<app>)))

(finalize)
