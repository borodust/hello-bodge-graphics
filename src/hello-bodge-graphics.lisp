(cl:defpackage :hello-bodge-graphics
  (:use :cl :cl-bodge.graphics)
  (:export run))
(cl:in-package :hello-bodge-graphics)

(defun make-rectangle-vertex-buffer ()
  (ge.gx:make-array-buffer #2a((0.5 0.0)
                               (0.5 0.5)
                               (0.0 0.0)
                               (0.0 0.5))))

(appkit:defapp hello-bodge-graphics ()
  ((pipeline :initform nil)
   (vertex-buffer :initform nil))
  (:viewport-title "Hello Graphics")
  (:viewport-width 800)
  (:viewport-height 600))

(defmethod appkit:configuration-flow ((this hello-bodge-graphics))
  (with-slots (pipeline vertex-buffer) this
    (for-graphics ()
      (setf pipeline (make-shader-pipeline 'pass-through-pipeline)
            vertex-buffer (make-rectangle-vertex-buffer)))))

(defmethod appkit:sweeping-flow ((this hello-bodge-graphics))
  (with-slots (pipeline vertex-buffer) this
    (for-graphics ()
      (ge.ng:dispose pipeline)
      (ge.ng:dispose vertex-buffer))))

(defun run ()
  (appkit:start 'hello-bodge-graphics))

(defmethod appkit:draw ((this hello-bodge-graphics))
  (with-slots (pipeline vertex-buffer) this
    (clear-rendering-output t :color (ge.ng:vec4 0.2 0.2 0.2 1))
    (render t pipeline :primitive :triangle-strip :vertex-count 4 'position vertex-buffer)))

(defshader (pass-through-vertex-shader
            (:sources "pass-through.vert.glsl")
            (:base-path :system-relative :hello-bodge-graphics "shaders/"))
  (position :name "v_Position"))

(defshader (pass-through-fragment-shader
            (:sources "pass-through.frag.glsl")
            (:base-path :system-relative :hello-bodge-graphics "shaders/")))

(defpipeline pass-through-pipeline
  :vertex pass-through-vertex-shader
  :fragment pass-through-fragment-shader)
