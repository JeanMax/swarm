(in-package :swarm)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 8) *boid-sight-range*))
(defparameter *boid-sight-range* 30
  "The height of the window/game (in pixels).")


(defun mean-direction (direction-list)
  (let ((n-directions (list-length direction-list)))
    (if (> n-directions 0)
        (labels ((mean-axis (axis-getter)
                   (round (reduce #'+ (mapcar axis-getter direction-list))
                          n-directions)))
          (make-instance 'point :x (mean-axis #'*x*) :y (mean-axis #'*y*)))
        (make-instance 'point :x 0 :y 0))))


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
  (let* ((neighbors
          (find-points-in-range self *boid-gang* *boid-sight-range*))
         (neighbors-directions
          (mapcar (lambda (n) (*previous-direction* n)) neighbors))
         (alignment-force
          (mean-direction neighbors-directions))
         (new-direction
          (mean-direction (list (*direction* self) alignment-force))))
    (set-coords (*direction* self) (*x* new-direction) (*y* new-direction))))
        ;; get forces f1, f2, f3
    ;; self.dir = mean(self.prev-dir, f1, f2, f3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *random-state* (make-random-state t))  ; random seed

(defun make-random-color ()
  "Return a randomly generated color."
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun random-interval (min-val max-val)
  "Return a random int in the interval [MIN-VAL, MAX-VAL]."
  (declare (type (signed-byte 16) min-val max-val))
  (+ (random (- max-val min-val -1)) min-val))

(defun make-random-boid ()
  "Return a fresh boid initialized with random attributes in a reasonable range."
  (let ((min-radius 3)
        (max-radius 5)
        (max-speed 4))
    (declare (type (unsigned-byte 4) min-radius max-radius max-speed))
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
