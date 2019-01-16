(cl:defpackage :hello-bodge-graphics/colored-triangle
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.appkit :hello-bodge-graphics))

(cl:in-package :hello-bodge-graphics/colored-triangle)

(defshader (colored-triangle-vertex-shader
            (:sources (merge-project-path "src/colored-triangle/vert.glsl")))
  ;; here we specify shader inputs

  ;; name of the position attribute in our vertex shader is, well, position
  (position :name "position")
  ;; color is color
  (color :name "color"))


(defshader (colored-triangle-fragment-shader
            (:sources (merge-project-path "src/colored-triangle/frag.glsl"))
            ;; our fragment shader doesn't have any inputs
            ))

(cl:in-package :hello-bodge-graphics/colored-triangle)

(defpipeline (colored-triangle-pipeline
              ;; explicitly tell our pipeline to treat our input as triangles
              (:primitive :triangles))
  :vertex colored-triangle-vertex-shader
  :fragment colored-triangle-fragment-shader)

(cl:in-package :hello-bodge-graphics/colored-triangle)

(defapp colored-triangle-graphics ()
  ((pipeline :initform nil)
   (position-buffer :initform nil)
   (color-buffer :initform nil))
  (:viewport-title "Hello Colorful Triangle")
  (:viewport-width 800)
  (:viewport-height 600))

(defmethod configuration-flow ((this colored-triangle-graphics))
  (with-slots (pipeline position-buffer color-buffer) this
    (for-graphics ()
      (setf pipeline (make-shader-pipeline 'colored-triangle-pipeline)
            position-buffer (make-array-buffer #2A((-0.5 -0.5)
                                                   (0.5 -0.5)
                                                   (0 0.5)))
            color-buffer (make-array-buffer #2A((1.0 0.0 0.0 1.0)
                                                (0.0 1.0 0.0 1.0)
                                                (0.0 0.0 1.0 1.0)))))))

(defmethod sweeping-flow ((this colored-triangle-graphics))
  (with-slots (pipeline position-buffer color-buffer) this
    (for-graphics ()
      (dispose pipeline)
      (dispose position-buffer)
      (dispose color-buffer))))

(defun run-example ()
  (start 'colored-triangle-graphics))

(export 'run-example)

(cl:in-package :hello-bodge-graphics/colored-triangle)

(defmethod draw ((this colored-triangle-graphics))
  (with-slots (pipeline color-buffer position-buffer) this
    (render t pipeline
            :vertex-count 3
            'position position-buffer
            'color color-buffer)))
