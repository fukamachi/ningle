# ningle

[![Build Status](https://travis-ci.org/fukamachi/ningle.svg?branch=master)](https://travis-ci.org/fukamachi/ningle)

"ningle" is a lightweight web application framework for Common Lisp.

## Usage

```common-lisp
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

(setf (ningle:route *app* "/login" :method :POST)
      #'(lambda (params)
          (if (authorize (cdr (assoc "username" params :test #'string=))
                         (cdr (assoc "password" params :test #'string=)))
              "Authorized!"
              "Failed...Try again.")))

(clack:clackup *app*)
```

Now you can access to http://localhost:5000/ and then ningle should show you "Welcome to ningle!".

## Installation

    (ql:quickload :ningle)

## Description

ningle is a fork project of [Caveman](http://fukamachi.github.com/caveman/). ningle doesn't require you to generate a project skeleton.

As this is a thin framework, you need to have subtle knowledge about [Clack](http://clacklisp.org). It is a server interface ningle bases on.

## Getting started

### Routing

ningle has the [Sinatra](http://www.sinatrarb.com/)-like routing system.

```common-lisp
;; GET request (default)
(setf (ningle:route *app* "/" :method :GET) ...)

;; POST request
(setf (ningle:route *app* "/" :method :POST) ...)

;; PUT request
(setf (ningle:route *app* "/" :method :PUT) ...)

;; DELETE request
(setf (ningle:route *app* "/" :method :DELETE) ...)

;; OPTIONS request
(setf (ningle:route *app* "/" :method :OPTIONS) ...)
```

Route pattern may contain "keyword" to put the value into the argument.

```common-lisp
(setf (ningle:route *app* "/hello/:name")
      #'(lambda (params)
          (format nil "Hello, ~A" (cdr (assoc :name params)))))
```

The above controller will be invoked when you access to "/hello/Eitaro" or "/hello/Tomohiro", and then `(cdr (assoc :name params))` will be "Eitaro" and "Tomohiro".

Route patterns may also contain "wildcard" parameters. They are accessible by `(assoc :splat params)`.

```common-lisp
(setf (ningle:route *app* "/say/*/to/*")
      #'(lambda (params)
          ; matches /say/hello/to/world
          (cdr (assoc :splat params)) ;=> ("hello" "world")
          ))

(setf (ningle:route *app* "/download/*.*")
      #'(lambda (params)
          ; matches /download/path/to/file.xml
          (cdr (assoc :splat params)) ;=> ("path/to/file" "xml")
          ))
```

Route matching with Regular Expressions:

```common-lisp
(setf (ningle:route *app* "/hello/([\\w]+)" :regexp t)
      #'(lambda (params)
          (format nil "Hello, ~A!" (first (cdr (assoc :captures params))))))
```

### Requirements

Routes may include a variety of matching conditions, such as the Accept:

```common-lisp
(setf (ningle:route *app* "/" :accept '("text/html" "text/xml"))
      #'(lambda (params)
          (declare (ignore params))
          "<html><body>Hello, World!</body></html>"))

(setf (ningle:route *app* "/" :accept "text/plain")
      #'(lambda (params)
          (declare (ignore params))
          "Hello, World!"))
```

You can easily define your own conditions:

```common-lisp
(setf (ningle:requirement *app* :probability)
      #'(lambda (value)
          (<= (random 100) value)))

(setf (ningle:route *app* "/win_a_car" :probability 10)
      #'(lambda (params)
          (declare (ignore params))
          "You won!"))

(setf (ningle:route *app* "/win_a_car")
      #'(lambda (params)
          (declare (ignore params))
          "Sorry, you lost."))
```

### Request & Response

ningle provides two special variables named `*request*` and `*response*`. They will be bound to an instance [Lack.Request](https://github.com/fukamachi/lack/blob/master/src/request.lisp#L33) and [Lack.Response](https://github.com/fukamachi/lack/blob/master/src/response.lisp#L19) for each request.

For example, by using them, you can change the response status code, Content-Type or something like that in each controllers.

```common-lisp
(setf (lack.response:response-headers *response*)
      (append (lack.response:response-headers *response*)
              (list :content-type "application/json")))

(setf (lack.response:response-headers *response*)
      (append (lack.response:response-headers *response*)
              (list :access-control-allow-origin "*")))

(setf (lack.response:response-status *response*) 201)
```

### Context

ningle provides an useful function named `context`. It is an accessor to an internal hash table.

```common-lisp
(setf (context :database)
      (dbi:connect :mysql
                   :database-name "test-db"
                   :username "nobody"
                   :password "nobody"))

(context :database)
;;=> #<DBD.MYSQL:<DBD-MYSQL-CONNECTION> #x3020013D1C6D>
```

### Using Session

ningle doesn't provide Session system in the core, but recommends to use [Lack.Middleware.Session](https://github.com/fukamachi/lack/blob/master/src/middleware/session.lisp#L20) with [Lack.Builder](https://github.com/fukamachi/lack/blob/master/src/builder.lisp#L62).

```common-lisp
(import 'lack.builder:builder)

(clack:clackup
  (builder
    :session
    *app*))
```

Of course, you can use other Lack Middlewares with ningle.

## See Also

* [Clack](http://clacklisp.org/)
* [Lack](https://github.com/fukamachi/lack)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
