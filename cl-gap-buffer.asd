(defpackage #:cl-gap-buffer-asd
  (:use #:cl #:asdf))

(in-package #:cl-gap-buffer-asd)

(defsystem cl-gap-buffer
  :name "cl-gap-buffer"
  :version "0.1"
  :author "Robert Smith"
  :maintainer "Robert Smith"
  :description "Gap buffers in Common Lisp."
  :long-description "An implementation of gap buffers in Common Lisp."

  :serial t
  :components ((:file "package")
               (:file "gap-buffer")))
