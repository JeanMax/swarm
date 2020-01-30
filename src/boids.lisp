(in-package :swarm)


;; TODO: find a better way for these
(defparameter *win-width* 1080
  "The width of the window/game (in pixels).")
(defparameter *win-height* 720
  "The height of the window/game (in pixels).")

(setf *random-state* (make-random-state t))  ; random seed


(defclass boid (circle vect)
  ((color
    :initarg :color
    :initform sdl:*white*
    :accessor *color*)))

(defmethod display ((self boid))
  (with-slots (x y radius color) self
    (sdl:draw-filled-circle-* x y radius
                              :color color)))


(defun make-random-color ()
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun random-interval (min-val max-val)
  (+ (random (- max-val min-val -1)) min-val))

(defun make-random-boid ()
  (let ((min-radius 3)
        (max-radius 7)
        (max-speed 5))
    (make-instance
     'boid
     :x (random-interval max-radius (- *win-width* max-radius))
     :y (random-interval max-radius (- *win-width* max-radius))
     :radius (random-interval min-radius max-radius)
     :color (make-random-color)
     :direction (make-instance
                 'point
                 :x (random-interval (- max-speed) max-speed)
                 :y (random-interval (- max-speed) max-speed)))))
