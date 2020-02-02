(in-package :swarm)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 16) *gang-size* *fps*))
(defparameter *gang-size* 256
  "The total number of boids simulated.")
(defparameter *boid-gang* (loop repeat *gang-size* collect (make-random-boid))
  "A list of boids to display on screen.")
(defparameter *super-boid* (make-instance 'boid :x 42 :y 42 :radius 10)
  "A special boid locked to the mouse coordinates.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SDL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *fps* 30
  "Frame Per Seconds: this set SDL event loop speed.")
(defparameter *textures-dir* "/home/mc/common-lisp/swarm/textures/" ;TODO
  "Where did you put these bmp files?")
(defparameter *background-image* nil
  "A preloaded background image. (Will be loaded in INIT-WINDOW)")


(defun init-window ()
  "Initialize the SDL window."
  (sdl:init-sdl)
  (sdl:window +world-width+ +world-height+
              :title-caption "Swarm"
              :flags '(sdl:sdl-hw-surface sdl:sdl-doublebuf))
  (setf *background-image*
        (sdl:load-image (concatenate 'string *textures-dir* "universe.bmp")))
  (setf (sdl:frame-rate) *fps*))

(defun frame-action ()
  "This function will be called each frame to handle all game logic (!graphics)."
  (reset-grid)
  (mapc #'move (cons *super-boid* *boid-gang*))
  (with-slots (x y direction) *super-boid*
    (declare (type (signed-byte 16) x y))
    ;; (set-coords direction (- x (sdl:mouse-x)) (- y (sdl:mouse-y)))
    (format t "dir: (~D, ~D)~&" (*x* direction) (*y* direction)) ;DEBUG
    (setf x (sdl:mouse-x))
    (setf y (sdl:mouse-y)))
  (mapc #'apply-forces (cons *super-boid* *boid-gang*)))

(defun redraw ()
  "Clear screen and redraw everything, then update display."
  ;; (sdl:clear-display sdl:*black*)
  (sdl:draw-surface *background-image*)
  (mapc #'display *boid-gang*)
  (display *super-boid*)
  (sdl:update-display))

(defun event-loop ()
  "Handle the SDL events (keyboard mostly)."
  (sdl:with-events ()
    (:quit-event () t)

    (:video-expose-event ()
                         (sdl:update-display))

    (:key-down-event (:key key)
                     (when (sdl:key= key :sdl-key-escape)
                       (sdl:push-quit-event)))

    (:mouse-button-down-event (:button button) ; :state state :x x :y y)
                              (when (sdl:key= button sdl:sdl-button-left)
                                (setf (*color* *super-boid*)
                                      (make-random-color))))

    (:idle ()

           (frame-action)
           (redraw)

           )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENTRY POINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun play ()
  "This is the entry point of the swarm simulator."
  (init-window)
  (event-loop)
  (sdl:quit-sdl))



;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples (* *fps* 10)
;;                           :report :flat
;;                           :loop t :reset t
;;                           :mode :cpu
;;                           :show-progress t)
;;   (dotimes (i 10000)(frame-action)))
