(in-package :swarm)


(defclass boid (circle vect)
  ((color
    :initarg :color
    :initform sdl:*white*
    :accessor *color*)))

(defmethod display ((self boid))
  (with-slots (x y radius color) self
    (sdl:draw-filled-circle-* x y radius
                              :color color)))


(setf *random-state* (make-random-state t))  ; random seed

(defun make-random-color ()
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun random-interval (min-val max-val)
  (+ (random (- max-val min-val -1)) min-val))

(defun make-random-boid ()
  (let ((min-radius 3)
        (max-radius 5)
        (max-speed 4))
    (make-instance
     'boid
     :x (random-interval max-radius (- *world-width* max-radius))
     :y (random-interval max-radius (- *world-width* max-radius))
     :radius (random-interval min-radius max-radius)
     :color (make-random-color)
     :direction (make-instance
                 'point
                 :x (random-interval (- max-speed) max-speed)
                 :y (random-interval (- max-speed) max-speed)))))
