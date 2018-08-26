(asdf:defsystem :hello-bodge-graphics
  :description "Guide to graphics subsystem of cl-bodge"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :mailto "dev@borodust.org"
  :depends-on (cl-bodge/graphics bodge-appkit)
  :pathname "src"
  :serial t
  :components ((:file "hello-bodge-graphics")))

(asdf:defsystem :hello-bodge-graphics/pass-through
  :description "Pass-through pipeline example for cl-bodge/graphics"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :mailto "dev@borodust.org"
  :depends-on (hello-bodge-graphics)
  :pathname "src/pass-through/"
  :serial t
  :components ((:file "app")))
