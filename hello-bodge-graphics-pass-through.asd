(asdf:defsystem :hello-bodge-graphics/pass-through
  :description "Pass-through pipeline example for cl-bodge/graphics"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :mailto "dev@borodust.org"
  :depends-on (hello-bodge-graphics)
  :pathname "src/pass-through/"
  :serial t
  :components (:file "app"))
