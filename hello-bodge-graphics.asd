(asdf:defsystem :hello-bodge-graphics
  :description "Guide to graphics subsystem of cl-bodge"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :mailto "dev@borodust.org"
  :depends-on (cl-bodge/graphics bodge-appkit)
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:module pass-through
                :components ((:file "app")))))
