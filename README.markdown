# ningle

"ningle" is a lightweight web application framework for Common Lisp.

## Usage

```common-lisp
(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

(setf (ningle:route *app* "/login" :method :POST)
      #'(lambda (params)
          (if (authorize (assoc "username" params :test #'string=)
                         (assoc "password" params :test #'string=))
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
          (format nil "Hello, ~A" (assoc "name" params :test #'string=))))
```

The above controller will be invoked when you access to "/hello/Eitaro" or "/hello/Tomohiro", and then `(assoc "name" params :test #'string=)` will be "Eitaro" and "Tomohiro".

Route patterns may also contain "wildcard" parameters. They are accessible by `(assoc :splat params)`.

```common-lisp
(setf (ningle:route *app* "/say/*/to/*")
      #'(lambda (params)
          ; matches /say/hello/to/world
          (assoc :splat params) ;=> ("hello" "world")
          ))

(setf (ningle:route *app* "/download/*.*")
      #'(lambda (params)
          ; matches /download/path/to/file.xml
          (assoc :splat params) ;=> ("path/to/file" "xml")
          ))
```

Route matching with Regular Expressions:

```common-lisp
(setf (ningle:route *app* "/hello/([\\w]+)" :regexp t)
      #'(lambda (params)
          (format nil "Hello, ~A!" (first (assoc :captures params)))))
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

ningle provides two special variables named `*request*` and `*response*`. They will be bound to an instance [Clack.Request](http://clacklisp.org/doc/clack.request.html) and [Clack.Response](http://clacklisp.org/doc/clack.response.html) for each request.

For example, by using them, you can change the response status code, Content-Type or something like that in each controllers.

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

ningle doesn't provide Session system in the core, but recommends to use [Clack.Middleware.Session](http://clacklisp.org/doc/clack.middleware.session.html) with [Clack.Builder](http://clacklisp.org/doc/clack.builder.html).

```common-lisp
(import 'clack.builder:builder
        'clack.middleware.session:<clack-middleware-session>)

(clack:clackup
  (builder
    <clack-middleware-session>
    *app*))
```

Of course, you can use other Clack Middlewares with ningle.

## See Also

* [Clack](http://clacklisp.org/)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
