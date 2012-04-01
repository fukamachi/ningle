# Ningle - Super micro framework for Common Lisp.

## Usage

    (use-package :ningle)
    (cl-syntax:use-syntax :annot)
    
    (defvar *app* (make-instance '<ningle-app>))
    
    @url GET "/"
    (defun index (params)
      @ignore params
      "Welcome to ningle!")
    
    (clack:clackup *app*)

## Description

ningle is a fork of [Caveman](http://clacklisp.org/caveman/). ningle doesn't require you to generate a project skeleton.

## Installation

    $ git clone https://github.com/fukamachi/ningle

    (ql:quickload :ningle)

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

