# hello-bodge-graphics

Step-by-step introduction into `cl-bodge` graphics system.

# Installing

```lisp
;; Install `cl-bodge` quicklisp distribution if not installed yet:
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :replace t :prompt nil)

;; Update main quicklisp dist just in case so no outdated packages got through during system loading
(ql:update-dist "quicklisp")
```

# Loading and running

```lisp
;; Load the example itself:
(ql:quickload :hello-bodge-graphics/pass-through)
;; And run it!
(hello-bodge-graphics/pass-through:run-example)
```

# Documentation, guide and sources

This system is written in literate programming style. Actual source code and documentation is
contained within [hello-bodge-graphics.org](hello-bodge-graphics.org) file which is also the
guide itself.
