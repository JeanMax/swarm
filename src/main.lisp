(in-package :swarm)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 16) *gang-size* *fps*))
(defparameter *gang-size* 500
  "The total number of boids simulated.")
(defparameter *boid-gang* (loop repeat *gang-size* collect (make-random-boid))
  "A list of boids to display on screen.")
(defparameter *super-boid* (make-instance 'boid :x 42 :y 42 :radius 30)
  "A special boid locked to the mouse coordinates.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SDL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *fps* 30
  "Frame Per Seconds: this set SDL event loop speed.")


(defun init-window ()
  "Initialize the SDL window."
  (sdl:init-sdl)
  (sdl:window *world-width* *world-height*
              :title-caption "Swarm"
              :flags '(sdl:sdl-hw-surface sdl:sdl-doublebuf))
  (setf (sdl:frame-rate) *fps*))

(defun frame-action ()
  "This function will be called each frame to handle all game logic (!graphics)."
  (mapc #'move *boid-gang*)
  (mapc #'apply-forces *boid-gang*)
  (with-slots (x y) *super-boid*
    (declare (type (signed-byte 16) x y))
    (setf x (sdl:mouse-x))
    (setf y (sdl:mouse-y))))

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
                              (when (sdl:key= button :sdl-button-left)
                                (setf (*color* *super-boid*)
                                      (make-random-color))))

    (:idle ()
           (frame-action)

           (sdl:clear-display sdl:*black*)

           (mapc #'display *boid-gang*)
           (display *super-boid*)

           (sdl:update-display))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENTRY POINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun play ()
  "This is the entry point of the swarm simulator."
  (init-window)
  (event-loop)
  (sdl:quit-sdl))
