(in-package :swarm)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 8) *boid-sight-range*))
(defparameter *boid-sight-range* 50
  "The height of the window/game (in pixels).")
(defparameter *boid-sight-color* (sdl:color :r 255 :a 20)
  "The height of the window/game (in pixels).")

(defun mean-coord (direction-list)
  (let ((n-directions (list-length direction-list)))
    (when (> n-directions 0)
        (labels ((mean-axis (axis-getter)
                   (round (reduce #'+ (mapcar axis-getter direction-list))
                          n-directions)))
          ; TODO: don't allocate a point each time
          (make-instance 'point :x (mean-axis #'*x*) :y (mean-axis #'*y*))))))

(defun alignment-force (neighbors)
  (mul
   (mean-coord
    (mapcar (lambda (n) (*previous-direction* n)) neighbors))
  2))

(defun cohesion-force (self neighbors)
  (mul (sub (mean-coord neighbors) self) 2))

(defun separation-force (self neighbors)
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (div (mean-coord (mapcar
                      (lambda (n) (sub (make-instance 'point :x l-x :y l-y) n))
                      neighbors))
         3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOID ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (sdl:draw-filled-circle-* x y *boid-sight-range* :color *boid-sight-color*)
    (sdl:draw-filled-circle-* x y radius :color color)))


(defmethod apply-forces ((self boid))
  (let ((neighbors
         (find-points-in-range self *boid-sight-range*))
         (new-direction nil))
    (unless (null neighbors)
      (setf new-direction
            (mean-coord (list (*direction* self)
                              (alignment-force neighbors)
                              (separation-force self neighbors)
                              (cohesion-force self neighbors))))
      (set-coords (*direction* self)
                  (rem (the integer (*x* new-direction)) +world-width+)  ; rem -> speed limit
                  (rem (the integer (*y* new-direction)) +world-height+)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RANDOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
     :x (random-interval max-radius (- +world-width+ max-radius))
     :y (random-interval max-radius (- +world-height+ max-radius))
     :radius (random-interval min-radius max-radius)
     :color (make-random-color)
     :direction (make-instance
                 'point
                 :x (random-interval (- max-speed) max-speed)
                 :y (random-interval (- max-speed) max-speed)))))
