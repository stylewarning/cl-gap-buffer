;;;; package.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

;;;; Declare the CL-GAP-BUFFER package.

(defpackage #:cl-gap-buffer
  (:use #:cl)
  (:export
   #:*gap-buffer-growth*                ; Variable

   #:gap-buffer                         ; Type
   
   #:make-gap-buffer                    ; Function
   #:gap-buffer-components
   #:gap-buffer-clear                   ; ...
   #:gap-buffer-length
   #:gap-buffer-string
   #:gap-buffer-insert-char
   #:gap-buffer-insert-string
   #:gap-buffer-move-left
   #:gap-buffer-move-right
   #:gap-buffer-backspace
   #:gap-buffer-delete)
  (:documentation "Gap buffer API."))