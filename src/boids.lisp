(in-package :swarm)

(defparameter *boid-sight-range* 10
  "The height of the window/game (in pixels).")


(defclass boid (circle vect)
  ((color
    :initarg :color
    :initform sdl:*white*
    :accessor *color*
    :type sdl:color
    :documentation "The color of the boid."))
  (:documentation "A boid (see: https://en.wikipedia.org/wiki/Boids)."))

(defmethod display ((self boid))
  "Display the given BOID on the sdl window."
  (with-slots (x y radius color) self
    (sdl:draw-filled-circle-* x y radius :color color)))

(defmethod apply-forces ((self boid))
  (let ((neighbors (find-points-in-range self *boid-gang* *boid-sight-range*)))
    ;; get forces f1, f2, f3
    ;; self.dir = mean(self.prev-dir, f1, f2, f3)
    )
  )


(setf *random-state* (make-random-state t))  ; random seed

(defun make-random-color ()
  "Return a randomly generated color."
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun random-interval (min-val max-val)
  "Return a random int in the interval [MIN-VAL, MAX-VAL]."
  (+ (random (- max-val min-val -1)) min-val))

(defun make-random-boid ()
  "Return a fresh boid initialized with random attributes in a reasonable range."
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
