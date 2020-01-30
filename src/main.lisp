(in-package :swarm)


(defparameter *gang-size* 500)
(defparameter *boid-gang* (loop repeat *gang-size* collect (make-random-boid)))
(defparameter *super-boid* (make-instance 'boid
                                          :x 42 :y 42 :radius 30))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SDL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *fps* 60
  "Frame Per Seconds: this set SDL event loop speed.")


(defun init-win ()
  "Initialize the SDL window."
  (sdl:window *win-width* *win-height*
              :title-caption "Swarm"
              :flags '(sdl:sdl-hw-surface sdl:sdl-doublebuf))
  (setf (sdl:frame-rate) *fps*))

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
           (mapc #'move *boid-gang*)
           (with-slots (x y) *super-boid*
             (setf x (sdl:mouse-x))
             (setf y (sdl:mouse-y)))

           (sdl:clear-display sdl:*black*)
           (mapc #'display *boid-gang*)
           (display *super-boid*)
           (sdl:update-display))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENTRY POINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun play ()
  "This is the entry point of the swarm simulator."
  (sdl:with-init ()
    (init-win)

    ;; (sdl:draw-surface (sdl:load-image (concatenate 'string
    ;;                                                *textures-dir* "xkcd.bmp")))
    ;; (sdl:update-display)

    (event-loop)))
