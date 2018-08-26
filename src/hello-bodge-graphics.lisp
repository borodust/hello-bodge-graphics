(cl:defpackage :hello-bodge-graphics
  (:use :cl :cl-bodge.engine :cl-bodge.graphics)
  (:export #:*project-path*
           #:merge-project-path))

(cl:in-package :hello-bodge-graphics)

(defparameter *project-path* nil)

(defun merge-project-path (relative-path)
  "Construct a full path joining *project-path* or a system-relative-pathname if *project-path*
is nil with provided relative path"
  (merge-pathnames relative-path (or *project-path*
                                     (asdf:system-relative-pathname :hello-bodge-graphics "./"))))
