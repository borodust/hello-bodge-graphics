(cl:defpackage :hello-bodge-graphics/pass-through
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.appkit :hello-bodge-graphics))

(cl:in-package :hello-bodge-graphics/pass-through)

(defshader (pass-through-vertex-shader
            ;; In :sources option we provide path to our shader.
            ;; Ensure shader sources you saved are put by this path relative to
            ;; *project-path* value or just put correct full path here
            (:sources (merge-project-path "src/pass-through/vert.glsl"))))


(defshader (pass-through-fragment-shader
            ;; Same as with vertex shader, here we let bodge know where to find the source
            (:sources (merge-project-path "src/pass-through/frag.glsl"))))

(cl:in-package :hello-bodge-graphics/pass-through)

;; This definition will tell bodge how to assemble pipeline.
;; Here we instruct it to compile and link shaders we defined earlier
;; with defshader macro.
(defpipeline pass-through-pipeline
  :vertex pass-through-vertex-shader
  :fragment pass-through-fragment-shader)

(cl:in-package :hello-bodge-graphics/pass-through)

;; Here we describe our little application we want to run our pass-through pipeline in
(defapp pass-through-graphics ()
  ;; this slot will hold our pipeline object
  ((pipeline :initform nil))
  ;; here go various application options
  (:viewport-title "Hello Pass-Through Graphics")
  (:viewport-width 800)
  (:viewport-height 600))

;; configuration-flow function is called every time application is initialized and reinitialized,
;; to initialize its state
;;
;; Why it is called *-flow, what are flows and what is this weird `for-graphics` macro
;; you can learn from the guide that describes core cl-bodge concepts
(defmethod configuration-flow ((this pass-through-graphics))
  (with-slots (pipeline) this
    (for-graphics ()
      ;; here we create our shader pipeline object we defined earlier
      (setf pipeline (make-shader-pipeline 'pass-through-pipeline)))))

;; sweeping-flow function is called every time applicatoin is reinitialized and is about to close
;; to let you release all acquired resources
(defmethod sweeping-flow ((this pass-through-graphics))
  (with-slots (pipeline) this
    (for-graphics ()
      ;; dispose is a bodge's universal function for freeing various engine resources
      ;; here we release resources taken by pipeline
      (dispose pipeline))))

;; This function starts our example application
(defun run-example ()
  (start 'pass-through-graphics))

;; Lets export it
(export 'run-example)

(cl:in-package :hello-bodge-graphics/pass-through)

;; #'appkit:draw is called every loop iteration with context bound to graphics system,
;; so we can use graphics functions w/o for-graphics flow redirection
(defmethod draw ((this pass-through-graphics))
  (with-slots (pipeline) this
    ;; Our rendering code, finally! Here we have default framebuffer as our target
    ;; and pipeline we created also telling to force primitive to :points,
    ;; letting it to know that we want to render 1 vertex, but not passing
    ;; any vertex data (letting underlying graphics driver to fill it with junk),
    ;; because we don't even need it in our pass-through shaders
    (render t pipeline :primitive :points :vertex-count 1)))
