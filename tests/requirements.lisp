(defpackage #:ningle/tests/requirements
  (:use #:cl
        #:ningle
        #:prove)
  (:import-from #:lack.test
                #:testing-app
                #:request)
  (:import-from #:lack.component
                #:to-app))
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

(testing-app (to-app *app*)
  (multiple-value-bind (body status)
      (request "/" :headers '((:accept . "text/plain")))
    (is body "Hello, World!")
    (is status 200))

  (multiple-value-bind (body status)
      (request "/" :headers '((:accept . "text/html")))
    (is body "<html><body>Hello, World!</body></html>")
    (is status 200))
  (is (nth-value 1
                 (request "/" :headers  '((:accept . "application/json"))))
      404))

(setf (requirement *app* :user-agent)
      (lambda (user-agent-regexp)
        (ppcre:scan-to-strings user-agent-regexp
                               (gethash "user-agent" (lack.request:request-headers *request*)))))

(setf (route *app* "/" :user-agent "Songbird/(\\d+\\.\\d+\\.\\d+)")
      (lambda (params)
        (format nil "Songbird ver ~A" (aref (cdr (assoc :user-agent params)) 0))))

(testing-app (to-app *app*)
  (is (nth-value 1 (request "/"))
      404)
  (multiple-value-bind (body status)
      (request "/" :headers '((:user-agent . "Songbird/2.2.0")))
    (is status 200)
    (is body "Songbird ver 2.2.0")))

(isnt (ningle.app::app-requirements (make-instance '<app>))
      (ningle.app::app-requirements (make-instance '<app>)))

(finalize)
