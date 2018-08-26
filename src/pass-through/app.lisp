(cl:in-package :hello-bodge-graphics)

(defshader (pass-through-vertex-shader
            ;; In :sources option we provide path our shader name.
            ;; It can be full path or relative path, but if you use relative path
            ;; you also need to provide :base-path.
            (:sources "vert.glsl")
            ;; It is better to use relative paths from the start,
            ;; so shader won't depend on you host machine.
            ;; Here we use :system-relative base path, which is merged from
            ;; component name of our ASDF system and provided relative path
            ;;
            ;; WARNING:
            ;; If you are evaluating guide code in the repl,
            ;; uncomment following form and put full path to your shader source directory there
            ;; (:base-path "/full/path/to/directory/containing/shader/sources/")
            ;; and instead wrap next line into a comment block
            (:base-path :system-relative :hello-bodge-graphics "pass-through/")))

(defshader (pass-through-fragment-shader
            ;; Same as with vertex shader, here we let bodge know how our source file is named
            (:sources "frag.glsl")
            ;; And here we tell bodge where to look for this file
            ;;
            ;; WARNING:
            ;; If you are evaluating guide code in the repl,
            ;; uncomment following form and put full path to your shader source directory there
            ;; (:base-path "/full/path/to/directory/containing/shader/sources/")
            ;; and instead wrap next line into a comment block
            (:base-path :system-relative :hello-bodge-graphics "pass-through/")))

(cl:in-package :hello-bodge-graphics)

;; This definition will tell bodge how to assemble pipeline.
;; Here we instruct it to compile and link shaders we defined earlier
;; with defshader macro.
(defpipeline pass-through-pipeline
  :vertex pass-through-vertex-shader
  :fragment pass-through-fragment-shader)

(cl:in-package :hello-bodge-graphics)

;; Here we describe our little application we want to run our pass-through pipeline in
(appkit:defapp pass-through-graphics ()
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
;; you can find in the guide that describes core cl-bodge concepts
(defmethod appkit:configuration-flow ((this pass-through-graphics))
  (with-slots (pipeline) this
    (for-graphics ()
      ;; here we create our shader pipeline object we defined earlier
      (setf pipeline (make-shader-pipeline 'pass-through-pipeline)))))


;; sweeping-flow function is called every time applicatoin is reinitialized and closing
;; to let you release all acquired resources
(defmethod appkit:sweeping-flow ((this pass-through-graphics))
  (with-slots (pipeline) this
    (for-graphics ()
      ;; dispose is a bodge's universal function for freeing various engine resources
      ;; here we release resources taken by pipeline
      (dispose pipeline))))

;; This function starts our example application
(defun run/pass-through-graphics ()
  (appkit:start 'pass-through-graphics))

;; Lets export it
(export 'run/pass-through-graphics)

(cl:in-package :hello-bodge-graphics)

;; #'appkit:draw is called every loop iteration with context bound to graphics system,
;; so we can use graphics functions w/o for-graphics flow redirection
(defmethod appkit:draw ((this pass-through-graphics))
  (with-slots (pipeline) this
    ;; Our rendering code, finally! Here we have default framebuffer as our target
    ;; and pipeline we created also telling to force primitive to :points,
    ;; letting it to know that we want to render 1 vertex, but not passing
    ;; any vertex data (letting underlying graphics driver to fill it with junk),
    ;; because we don't even need it in our pass-through shaders
    (render t pipeline :primitive :points :vertex-count 1)))
