(cl:in-package :hello-bodge-graphics)

(defparameter *project-path* nil)

(defun merge-project-path (relative-path)
  "Construct a full path joining *project-path* with provided relative path"
  (merge-pathnames relative-path (or *project-path*
                                     (asdf:system-relative-pathname :hello-bodge-graphics "./"))))
